(ns clj-kondo.impl.analyzer
  {:no-doc true}
  (:refer-clojure :exclude [ns-name])
  (:require
   [clj-kondo.impl.analyzer.babashka :as babashka]
   [clj-kondo.impl.analyzer.clojure-data-xml :as xml]
   [clj-kondo.impl.analyzer.common :refer [common]]
   [clj-kondo.impl.analyzer.compojure :as compojure]
   [clj-kondo.impl.analyzer.core-async :as core-async]
   [clj-kondo.impl.analyzer.datalog :as datalog]
   [clj-kondo.impl.analyzer.jdbc :as jdbc]
   [clj-kondo.impl.analyzer.match :as match]
   [clj-kondo.impl.analyzer.namespace :as namespace-analyzer
    :refer [analyze-ns-decl]]
   [clj-kondo.impl.analyzer.potemkin :as potemkin]
   [clj-kondo.impl.analyzer.re-frame :as re-frame]
   [clj-kondo.impl.analyzer.spec :as spec]
   [clj-kondo.impl.analyzer.test :as test]
   [clj-kondo.impl.analyzer.usages :as usages :refer [analyze-usages2]]
   [clj-kondo.impl.config :as config]
   [clj-kondo.impl.findings :as findings]
   [clj-kondo.impl.hooks :as hooks]
   [clj-kondo.impl.linters :as linters]
   [clj-kondo.impl.linters.deps-edn :as deps-edn]
   [clj-kondo.impl.linters.keys :as key-linter]
   [clj-kondo.impl.macroexpand :as macroexpand]
   [clj-kondo.impl.metadata :as meta]
   [clj-kondo.impl.namespace :as namespace :refer [resolve-name]]
   [clj-kondo.impl.parser :as p]
   [clj-kondo.impl.rewrite-clj.parser.token :refer [*invalid-token-exceptions*]]
   [clj-kondo.impl.schema :as schema]
   [clj-kondo.impl.types :as types]
   [clj-kondo.impl.utils :as utils :refer
    [symbol-call node->line parse-string tag select-lang deep-merge one-of
     linter-disabled? tag sexpr string-from-token assoc-some ctx-with-bindings]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [sci.core :as sci]))

(set! *warn-on-reflection* true)

(declare analyze-expression**)

(defn analyze-children
  ([ctx children]
   (analyze-children ctx children true))
  ([{:keys [:callstack :config :top-level?] :as ctx} children add-new-arg-types?]
   (let [top-level? (and top-level?
                         (let [fst (first callstack)]
                           (one-of fst [[clojure.core comment]
                                        [cljs.core comment]
                                        [clojure.core do]
                                        [cljs.core do]
                                        [clojure.core let]
                                        [cljs.core let]])))]
     (when-not (and (:in-comment ctx)
                    (:skip-comments config))
       (let [len (count children)
             ctx (assoc ctx
                        :top-level? top-level?
                        :arg-types (if add-new-arg-types?
                                     (let [[k v] (first callstack)]
                                       (if (and (symbol? k)
                                                (symbol? v))
                                         (atom [])
                                         nil))
                                     (:arg-types ctx))
                        :len len)]
         (into []
               (comp (map-indexed (fn [i e]
                                    (analyze-expression** (assoc ctx :idx i) e)))
                     cat)
               children))))))

(defn analyze-keys-destructuring-defaults [ctx prev-ctx m defaults opts]
  (let [skip-reg-binding? (or (:skip-reg-binding? ctx)
                              (when (:fn-args? opts)
                                (-> ctx :config :linters :unused-binding
                                    :exclude-destructured-keys-in-fn-args)))]
    (when-not skip-reg-binding?
      (doseq [[k _v] (partition 2 (:children defaults))
              :let [sym (:value k)
                    mta (meta k)]
              :when sym]
        (if-let [binding (get m sym)]
          (namespace/reg-destructuring-default! ctx mta binding)
          (findings/reg-finding!
           ctx
           {:message (str sym " is not bound in this destructuring form") :level :warning
            :row (:row mta)
            :col (:col mta)
            :end-row (:end-row mta)
            :end-col (:end-col mta)
            :filename (:filename ctx)
            :type :unbound-destructuring-default})))))
  (doseq [[k v] (partition 2 (:children defaults))]
    (when-not (and (identical? :token (utils/tag k))
                   (simple-symbol? (:value k)))
      (let [m (meta k)]
        (findings/reg-finding!
         ctx
         {:message "Keys in :or should be simple symbols."
          :level :error
          :row (:row m)
          :col (:col m)
          :end-row (:end-row m)
          :end-col (:end-col m)
          :filename (:filename ctx)
          :type :syntax})))
    (if (= k v)
      ;; see #915
      (analyze-expression** prev-ctx v)
      (analyze-expression** ctx v))))

(defn ctx-with-linter-disabled [ctx linter]
  (assoc-in ctx [:config :linters linter :level] :off))

(defn ctx-with-linters-disabled [ctx linters]
  (let [config (get ctx :config)
        linters-config (get config :linters)
        linters-config (reduce (fn [linters linter]
                                 (assoc-in linters [linter :level] :off))
                               linters-config linters)
        config (assoc config :linters linters-config)
        ctx (assoc ctx :config config)]
    ctx))

(defn lift-meta-content*
  "Used within extract-bindings. Disables unresolved symbols while
  linting metadata."
  [{:keys [:lang] :as ctx} expr]
  (meta/lift-meta-content2
   (if (= :cljs lang)
     (ctx-with-linter-disabled ctx :unresolved-symbol)
     ctx)
   expr))

(defn scope-end
  "Used within extract-bindings. Extracts the end postion of the
   scoped-expr for the binding."
  [scoped-expr]
  (some-> scoped-expr
          meta
          (select-keys [:end-row :end-col])
          (set/rename-keys {:end-row :scope-end-row :end-col :scope-end-col})))

(defn extract-bindings
  ([ctx expr] (extract-bindings ctx expr expr {}))
  ([ctx expr scoped-expr opts]
   (when expr
     (utils/handle-ignore ctx expr)
     (let [fn-args? (:fn-args? opts)
           keys-destructuring? (:keys-destructuring? opts)
           expr (lift-meta-content* ctx expr)
           t (tag expr)
           skip-reg-binding? (or (:skip-reg-binding? ctx)
                                 (when (and keys-destructuring? fn-args?)
                                   (-> ctx :config :linters :unused-binding
                                       :exclude-destructured-keys-in-fn-args)))]
       (case t
         :token
         (cond
           ;; symbol
           (utils/symbol-token? expr)
           (let [sym (:value expr)]
             (when (= (:destructuring-type opts) :keys)
               (usages/analyze-keyword ctx expr opts))
             (when (not= '& sym)
               (let [ns (namespace sym)
                     valid? (or (not ns)
                                keys-destructuring?)]
                 (if valid?
                   (let [s (symbol (name sym))
                         m (meta expr)
                         t (or (types/tag-from-meta (:tag m))
                               (:tag opts))
                         v (cond-> (assoc m
                                          :name s
                                          :filename (:filename ctx)
                                          :tag t)
                             (:analyze-locals? ctx)
                             (-> (assoc :id (swap! (:id-gen ctx) inc)
                                        :str (str expr))
                                 (merge (scope-end scoped-expr))))]
                     (when-not skip-reg-binding?
                       (namespace/reg-binding! ctx
                                               (-> ctx :ns :name)
                                               v))
                     (namespace/check-shadowed-binding! ctx s expr)
                     (with-meta {s v} (when t {:tag t})))
                   (findings/reg-finding!
                    ctx
                    (node->line (:filename ctx)
                                expr
                                :error
                                :syntax
                                (str "unsupported binding form " sym)))))))
           ;; keyword
           (:k expr)
           (let [k (:k expr)]
             (usages/analyze-keyword ctx expr opts)
             (if keys-destructuring?
               (let [s (-> k name symbol)
                     m (meta expr)
                     v (cond-> (assoc m
                                      :name s
                                      :filename (:filename ctx))
                         (:analyze-locals? ctx)
                         (-> (assoc :id (swap! (:id-gen ctx) inc)
                                    :str (str expr))
                             (merge (scope-end scoped-expr))))]
                 (when-not skip-reg-binding?
                   (namespace/reg-binding! ctx
                                           (-> ctx :ns :name)
                                           v))
                 {s v})
               ;; TODO: we probably need to check if :as is supported in this
               ;; context, e.g. seq-destructuring?
               (when (not= :as k)
                 (findings/reg-finding!
                  ctx
                  (node->line (:filename ctx)
                              expr
                              :error
                              :syntax
                              (str "unsupported binding form " k))))))
           :else
           (findings/reg-finding!
            ctx
            (node->line (:filename ctx)
                        expr
                        :error
                        :syntax
                        (str "unsupported binding form " expr))))
         :vector (let [children (:children expr)
                       all-tokens? (every? #(identical? :token %) (map :tag children))
                       v (let [ctx (update ctx :callstack conj [nil :vector])]
                           (if all-tokens?
                             (map #(extract-bindings ctx % scoped-expr opts) children)
                             (-> (reduce (fn [[ctx acc] expr]
                                           (let [bnds (extract-bindings ctx expr scoped-expr opts)]
                                             [(ctx-with-bindings ctx bnds) (conj! acc bnds)]))
                                         [ctx
                                          (transient [])]
                                         (:children expr))
                                 second
                                 persistent!)))
                       tags (map :tag (map meta v))
                       expr-meta (meta expr)
                       t (:tag expr-meta)
                       t (when t (types/tag-from-meta t))]
                   (with-meta (into {} v)
                     ;; this is used for checking the return tag of a function body
                     (assoc expr-meta
                            :tag t
                            :tags tags)))
         :namespaced-map (extract-bindings ctx (first (:children expr)) scoped-expr opts)
         :map
         ;; first check even amount of keys + vals
         (do (key-linter/lint-map-keys ctx expr)
             (loop [[k v & rest-kvs] (:children expr)
                    res {}]
               (if k
                 (let [k (lift-meta-content* ctx k)]
                   (cond (:k k)
                         (do
                           (analyze-usages2 ctx k)
                           (case (keyword (name (:k k)))
                             (:keys :syms :strs)
                             (recur rest-kvs
                                    (into res (map #(extract-bindings
                                                     ctx
                                                     %
                                                     scoped-expr
                                                     (assoc opts
                                                            :keys-destructuring? true
                                                            :destructuring-type (some-> k :k name keyword)
                                                            :destructuring-expr k)))
                                          (:children v)))
                             ;; or doesn't introduce new bindings, it only gives defaults
                             :or
                             (if (empty? rest-kvs)
                               ;; or can refer to a binding introduced by what we extracted
                               (let [prev-ctx ctx
                                     ctx (ctx-with-bindings ctx res)]
                                 (analyze-keys-destructuring-defaults ctx prev-ctx res v opts)
                                 (recur rest-kvs res))
                               ;; analyze or after the rest
                               (recur (concat rest-kvs [k v]) res))
                             :as (if (-> ctx :config :linters :unused-binding
                                         :exclude-destructured-as)
                                   (recur rest-kvs (merge res (extract-bindings (assoc ctx :skip-reg-binding? true) v scoped-expr opts)))
                                   (recur rest-kvs (merge res (extract-bindings ctx v scoped-expr opts))))
                             (recur rest-kvs res)))
                         :else
                         (recur rest-kvs (merge res
                                                (extract-bindings ctx k scoped-expr opts)
                                                {:analyzed (analyze-expression** ctx v)}))))
                 res)))
         (findings/reg-finding!
          ctx
          (node->line (:filename ctx)
                      expr
                      :error
                      :syntax
                      (str "unsupported binding form " expr))))))))

(defn analyze-in-ns [ctx {:keys [:children] :as expr}]
  (let [{:keys [:row :col]} expr
        lang (:lang ctx)
        ns-name (-> children second :children first :value)
        ns (when ns-name
             (-> (namespace-analyzer/new-namespace
                  (:filename ctx)
                  (:base-lang ctx)
                  (:lang ctx)
                  ns-name
                  :in-ns
                  row col)
                 ;; fix fully qualified core references
                 (assoc :qualify-ns (cond-> {}
                                      (identical? :clj lang)
                                      (assoc 'clojure.core 'clojure.core)
                                      (identical? :cljs lang)
                                      (assoc 'cljs.core 'cljs.core
                                             'clojure.core 'cljs.core)))))]
    (namespace/reg-namespace! ctx ns)
    (analyze-children ctx (next children))
    ns))

;;;; function arity

(defn analyze-arity [sexpr]
  (loop [[arg & rest-args] sexpr
         arity 0]
    (if arg
      (if (= '& arg)
        {:min-arity arity
         :varargs? true}
        (recur rest-args
               (inc arity)))
      {:fixed-arity arity})))

(defn analyze-fn-arity [ctx body]
  (if-let [children (not-empty (:children body))]
    (let [arg-vec (first children)
          arg-vec-t (tag arg-vec)]
      (if (not= :vector arg-vec-t)
        (findings/reg-finding! ctx
                               (node->line (:filename ctx)
                                           body
                                           :warning
                                           :syntax
                                           "Function arguments should be wrapped in vector."))
        (let [arg-bindings (extract-bindings ctx arg-vec (:fn-body body) {:fn-args? true})
              {return-tag :tag
               arg-tags :tags} (meta arg-bindings)
              arg-list (sexpr arg-vec)
              arity (analyze-arity arg-list)
              ret (cond-> {:arg-bindings (dissoc arg-bindings :analyzed)
                           :arity arity
                           :analyzed-arg-vec (:analyzed arg-bindings)
                           :args arg-tags
                           :ret return-tag}
                    (:analyze-arglists? ctx) (assoc :arglist-str (str arg-vec)))]
          ret)))
    (findings/reg-finding! ctx
                           (node->line (:filename ctx)
                                       body
                                       :warning
                                       :syntax
                                       "Invalid function body."))))

(defn analyze-pre-post-map [ctx expr]
  (let [children (:children expr)]
    (key-linter/lint-map-keys ctx expr)
    (mapcat (fn [[key-expr value-expr]]
              (let [analyzed-key (analyze-expression** ctx key-expr)
                    analyzed-value (if (= :post (:k key-expr))
                                     (analyze-expression**
                                      (ctx-with-bindings ctx '{% {}}) value-expr)
                                     (analyze-expression** ctx value-expr))]
                (concat analyzed-key analyzed-value)))
            (partition 2 children))))

(defn analyze-fn-body [ctx body]
  (let [docstring (:docstring ctx)
        macro? (:macro? ctx)
        {:keys [:arg-bindings
                :arity :analyzed-arg-vec :arglist-str]
         return-tag :ret
         arg-tags :args} (analyze-fn-arity ctx body)
        ctx (ctx-with-bindings ctx arg-bindings)
        ctx (assoc ctx
                   :recur-arity arity
                   :top-level? false)
        children (next (:children body))
        first-child (first children)
        one-child? (= 1 (count children))
        pre-post-map (when-not one-child?
                       (when (and first-child
                                  (identical? :map (tag first-child)))
                         first-child))
        analyze-pre-post (when pre-post-map
                           (analyze-pre-post-map ctx first-child))
        children (if pre-post-map (next children) children)
        _
        (let [t (when first-child (tag first-child))]
          (cond (and (not docstring)
                     (not one-child?)
                     (one-of t [:token :multi-line])
                     (string-from-token first-child))
                (findings/reg-finding! ctx
                                       (node->line (:filename ctx)
                                                   first-child
                                                   :warning
                                                   :misplaced-docstring
                                                   "Misplaced docstring."))))
        [parsed return-tag]
        (if (or macro? return-tag)
          [(analyze-children ctx children) return-tag]
          (let [last-expr (last children)
                ret-expr-id (gensym)
                last-expr (when last-expr (assoc last-expr :id ret-expr-id))
                body-exprs (concat (butlast children) [last-expr])
                parsed (doall (analyze-children ctx body-exprs))
                ret-tag (or
                         return-tag
                         (let [maybe-call (get @(:calls-by-id ctx) ret-expr-id)
                               tag (cond maybe-call (:ret maybe-call)
                                         last-expr (types/expr->tag ctx last-expr))]
                           tag))]
            [parsed ret-tag]))]
    (assoc arity
           :parsed
           (concat analyzed-arg-vec analyze-pre-post parsed)
           :ret return-tag
           :arglist-str arglist-str
           :args arg-tags)))

(defn fn-bodies [ctx children body]
  (loop [[expr & rest-exprs :as exprs] children]
    (when expr
      (let [expr (meta/lift-meta-content2 ctx expr)
            t (tag expr)]
        (case t
          :vector [{:children exprs :fn-body body}]
          :list exprs
          (recur rest-exprs))))))

(defn analyze-defn [ctx expr defined-by]
  (let [ns-name (-> ctx :ns :name)
        ;; "my-fn docstring" {:no-doc true} [x y z] x
        [name-node & children] (next (:children expr))
        name-node (when name-node (meta/lift-meta-content2 ctx name-node))
        fn-name (:value name-node)
        call (name (symbol-call expr))
        var-meta (meta name-node)
        meta-node (when-let [fc (first children)]
                    (let [t (tag fc)]
                      (if (= :map t) fc
                          (when (not= :vector t)
                            (when-let [sc (second children)]
                              (when (= :map (tag sc))
                                sc))))))
        ;; use dorun to force evaluation, we don't use the result!
        _ (when meta-node (dorun (analyze-expression** ctx meta-node)))
        var-meta (if meta-node
                   (merge var-meta
                          (sexpr meta-node))
                   var-meta)
        macro? (or (= "defmacro" call)
                   (:macro var-meta))
        deprecated (:deprecated var-meta)
        ctx (if macro?
              (ctx-with-bindings ctx '{&env {}
                                       &form {}})
              ctx)
        private? (or (= "defn-" call)
                     (:private var-meta))
        docstring (or (string-from-token (first children))
                      (some-> var-meta :doc str))
        bodies (fn-bodies ctx children expr)
        _ (when (empty? bodies)
            (findings/reg-finding! ctx
                                   (node->line (:filename ctx)
                                               expr
                                               :warning
                                               :syntax
                                               "Invalid function body.")))
        ;; var is known when making recursive call
        _ (when fn-name
            (namespace/reg-var!
             ctx ns-name fn-name expr {:temp true}))
        parsed-bodies (map #(analyze-fn-body
                             (-> ctx
                                 (assoc :docstring docstring
                                        :in-def fn-name
                                        :macro? macro?))
                             %)
                           bodies)
        arities (into {} (map (fn [{:keys [:fixed-arity :varargs? :min-arity :ret :args]}]
                                (let [arg-tags (when (some identity args)
                                                 args)
                                      v (assoc-some {}
                                                    :ret ret :min-arity min-arity
                                                    :args arg-tags)]
                                  (if varargs?
                                    [:varargs v]
                                    [fixed-arity v]))))
                      parsed-bodies)
        fixed-arities (into #{} (filter number?) (keys arities))
        varargs-min-arity (get-in arities [:varargs :min-arity])
        arglist-strs (mapv :arglist-str parsed-bodies)]
    (when fn-name
      (namespace/reg-var!
       ctx ns-name fn-name expr
       (assoc-some (meta name-node)
                   :macro macro?
                   :private private?
                   :deprecated deprecated
                   :defined-by defined-by
                   :fixed-arities (not-empty fixed-arities)
                   :arglist-strs (when (:analyze-arglists? ctx)
                                   arglist-strs)
                   :output-schema-str (when (:output-schema name-node)
                                        (str (sexpr (:output-schema name-node))))
                   :arities arities
                   :varargs-min-arity varargs-min-arity
                   :doc docstring
                   :added (:added var-meta))))
    (mapcat :parsed parsed-bodies)))

(defn analyze-case [ctx expr]
  (let [children (rest (:children expr))
        matched-val (first children)]
    (analyze-expression** ctx matched-val)
    (loop [[constant expr & exprs] (rest children)]
      (when constant
        (if-not expr
          ;; this is the default expression
          (analyze-expression** ctx constant)
          (do (let [t (tag constant)]
                (if (identical? :list t)
                  (run! #(analyze-usages2 ctx % {:quote? true}) (:children constant))
                  (analyze-usages2 ctx constant {:quote? true})))
              (when expr
                (analyze-expression** ctx expr)
                (recur exprs))))))))

(defn expr-bindings [ctx binding-vector scoped-expr]
  (let [ctx (update ctx :callstack conj [:nil :vector])]
    (->> binding-vector :children
         (take-nth 2)
         (map #(extract-bindings ctx % scoped-expr {}))
         (reduce deep-merge {}))))

(defn analyze-let-like-bindings [ctx binding-vector scoped-expr]
  (let [resolved-as-clojure-var-name (:resolved-as-clojure-var-name ctx)
        for-like? (one-of resolved-as-clojure-var-name [for doseq])
        callstack (:callstack ctx)
        call (-> callstack second second)
        let? (= 'let call)
        ;; don't register arg types on the same level
        ctx (assoc ctx :arg-types (atom []))]
    (loop [[binding value & rest-bindings] (-> binding-vector :children)
           bindings (:bindings ctx)
           arities (:arities ctx)
           analyzed []]
      (if binding
        (let [binding-tag (tag binding)
              binding-val (case binding-tag
                            :token (or
                                    ;; symbol
                                    (:value binding)
                                    ;; keyword
                                    (:k binding))
                            nil)
              ;; binding-sexpr (sexpr binding)
              for-let? (and for-like?
                            (= :let binding-val))]
          (if for-let?
            (let [{new-bindings :bindings
                   new-analyzed :analyzed
                   new-arities :arities}
                  (analyze-let-like-bindings
                   (ctx-with-bindings ctx bindings) value scoped-expr)]
              (recur rest-bindings
                     (merge bindings new-bindings)
                     (merge arities new-arities)
                     (concat analyzed new-analyzed)))
            (let [binding (cond for-let? value
                                ;; ignore :when and :while in for
                                (keyword? binding-val) nil
                                :else binding)
                  ctx* (-> ctx
                           (ctx-with-bindings bindings)
                           (update :arities merge arities))
                  value-id (gensym)
                  analyzed-value (when (and value (not for-let?))
                                   (analyze-expression** ctx* (assoc value :id value-id)))
                  tag (when (and let? binding (= :token (tag binding)))
                        (let [maybe-call (get @(:calls-by-id ctx) value-id)]
                          (cond maybe-call (:ret maybe-call)
                                value (types/expr->tag ctx* value))))
                  new-bindings (when binding (extract-bindings ctx* binding scoped-expr {:tag tag}))
                  analyzed-binding (:analyzed new-bindings)
                  new-bindings (dissoc new-bindings :analyzed)
                  next-arities (if-let [arity (:arity (meta analyzed-value))]
                                 ;; in this case binding-sexpr is a symbol,
                                 ;; since functions cannot be destructured
                                 (assoc arities binding-val arity)
                                 arities)]
              (recur rest-bindings
                     (merge bindings new-bindings)
                     next-arities (concat analyzed analyzed-binding analyzed-value)))))
        {:arities arities
         :bindings bindings
         :analyzed analyzed}))))

(defn lint-even-forms-bindings! [ctx form-name bv]
  (let [num-children (count (:children bv))]
    (when (odd? num-children)
      (findings/reg-finding!
       ctx
       (node->line (:filename ctx) bv :error :syntax
                   (format "%s binding vector requires even number of forms" form-name))))))

(defn assert-vector [ctx call expr]
  (when expr
    (let [vec? (and expr (= :vector (tag expr)))]
      (if-not vec?
        (do (findings/reg-finding!
             ctx
             (node->line (:filename ctx) expr :error :syntax
                         ;; cf. error in clojure
                         (format "%s requires a vector for its binding" call)))
            nil)
        expr))))

(defn analyze-like-let
  [{:keys [:filename :callstack
           :let-parent] :as ctx} expr]
  (let [call (-> callstack first second)
        [current-call parent-call] callstack
        parent-let (one-of parent-call
                           [[clojure.core let]
                            [cljs.core let]])
        current-let (one-of current-call
                            [[clojure.core let]
                             [cljs.core let]])
        bv-node (-> expr :children second)
        valid-bv-node (assert-vector ctx call bv-node)]
    (when (and current-let
               (not (:clj-kondo.impl/generated expr))
               (or (and parent-let (= parent-let let-parent)
                        ;; not generated by hook code
                        (not (:clj-kondo.impl/generated (meta parent-let))))
                   (and valid-bv-node (empty? (:children valid-bv-node)))))
      (findings/reg-finding!
       ctx
       (node->line filename expr :warning :redundant-let "Redundant let expression.")))
    (when bv-node
      (let [{analyzed-bindings :bindings
             arities :arities
             analyzed :analyzed}
            (analyze-let-like-bindings
             (-> ctx
                 ;; prevent linting redundant let when using let in bindings
                 (update :callstack #(cons [nil :let-bindings] %))) valid-bv-node expr)
            let-body (nnext (:children expr))
            let-parent (when (and current-let (= 1 (count let-body)))
                         current-let)
            _ (lint-even-forms-bindings! ctx call valid-bv-node)
            analyzed (concat analyzed
                             (doall
                              (analyze-children
                               (-> ctx
                                   (ctx-with-bindings analyzed-bindings)
                                   (update :arities merge arities)
                                   (assoc :let-parent let-parent))
                               let-body
                               false)))]
        analyzed))))

(defn analyze-do [{:keys [:filename :callstack] :as ctx} expr]
  (let [parent-call (second callstack)
        core? (one-of (first parent-call) [clojure.core cljs.core])
        core-sym (when core?
                   (second parent-call))
        ;; avoid warnings from hook code
        generated? (:clj-kondo.impl/generated expr)
        redundant?
        (and (not generated?)
             (not= 'fn* core-sym)
             (not= 'let* core-sym)
             (or
              ;; zero or one children
              (< (count (rest (:children expr))) 2)
              (and core?
                   (not (:clj-kondo.impl/generated (meta parent-call)))
                   (or
                    ;; explicit do
                    (= 'do core-sym)
                    ;; implicit do
                    (one-of core-sym [fn defn defn-
                                      let when-let loop binding with-open
                                      doseq try when when-not when-first
                                      when-some future])))))]
    (when redundant?
      (findings/reg-finding!
       ctx
       (node->line filename expr :warning :redundant-do "redundant do"))))
  (analyze-children ctx (next (:children expr))))

(defn lint-two-forms-binding-vector! [ctx form-name expr]
  (let [num-children (count (:children expr))]
    (when (not= 2 num-children)
      (findings/reg-finding!
       ctx
       (node->line (:filename ctx) expr :error :syntax (format "%s binding vector requires exactly 2 forms" form-name))))))

(defn analyze-conditional-let [ctx call expr]
  (let [children (next (:children expr))
        bv (first children)
        vec? (when bv (= :vector (tag bv)))]
    (when vec?
      (let [if? (one-of call [if-let if-some])
            condition (-> bv :children second)
            body-exprs (next children)
            bindings (expr-bindings ctx bv (if if? (first body-exprs) expr))
            ctx-with-binding (ctx-with-bindings ctx
                                                (dissoc bindings
                                                        :analyzed))]
        (lint-two-forms-binding-vector! ctx call bv)
        (concat (:analyzed bindings)
                (analyze-expression** ctx condition)
                (if if?
                  ;; in the case of if, the binding is only valid in the first expression
                  (concat
                   (analyze-expression** (ctx-with-bindings ctx
                                                            (dissoc bindings
                                                                    :analyzed))
                                         (first body-exprs))
                   (analyze-children ctx (rest body-exprs) false))
                  (analyze-children ctx-with-binding body-exprs false)))))))

(defn fn-arity [ctx bodies]
  (let [arities (map #(analyze-fn-arity ctx %) bodies)
        fixed-arities (set (keep (comp :fixed-arity :arity) arities))
        varargs-min-arity (some #(when (:varargs? (:arity %))
                                   (:min-arity (:arity %))) arities)]
    (cond-> {}
      (seq fixed-arities) (assoc :fixed-arities fixed-arities)
      varargs-min-arity (assoc :varargs-min-arity varargs-min-arity))))

(defn analyze-fn [ctx expr]
  (let [children (:children expr)
        ?fn-name (when-let [?name-expr (second children)]
                   (when-let [n (utils/symbol-from-token ?name-expr)]
                     n))
        bodies (fn-bodies ctx (next children) expr)
        ;; we need the arity beforehand because this is valid in each body
        arity (fn-arity (assoc ctx :skip-reg-binding? true) bodies)
        parsed-bodies
        (map #(analyze-fn-body
               (if ?fn-name
                 (-> ctx
                     (update :bindings conj [?fn-name
                                             (assoc (meta (second children))
                                                    :name ?fn-name
                                                    :filename (:filename ctx))])
                     (update :arities assoc ?fn-name
                             arity))
                 ctx) %) bodies)]
    (with-meta (mapcat :parsed parsed-bodies)
      {:arity arity})))

(defn analyze-alias [ctx expr]
  (let [ns (:ns ctx)
        [alias-expr ns-expr :as children] (rest (:children expr))
        alias-sym
        (let [t (tag alias-expr)]
          (or (when (identical? :quote t)
                (:value (first (:children alias-expr))))
              (when (identical? :list t)
                (let [children (:children alias-expr)]
                  (when (= 'quote (some-> children first
                                          utils/symbol-from-token))
                    (utils/symbol-from-token (second children)))))))
        ns-sym
        (let [t (tag ns-expr)]
          (or (when (identical? :quote t)
                (:value (first (:children ns-expr))))
              (when (identical? :list t)
                (let [children (:children ns-expr)]
                  (when (= 'quote (some-> children first
                                          utils/symbol-from-token))
                    (utils/symbol-from-token (second children)))))))]
    (if (and alias-sym (symbol? alias-sym) ns-sym (symbol? ns-sym))
      (namespace/reg-alias! ctx (:name ns) alias-sym ns-sym)
      (analyze-children ctx children))
    (assoc-in ns [:qualify-ns alias-sym] ns-sym)))

(defn analyze-loop [ctx expr]
  (let [bv (-> expr :children second)]
    (when (and bv (= :vector (tag bv)))
      (let [arg-count (let [c (count (:children bv))]
                        (when (even? c)
                          (/ c 2)))]
        (analyze-like-let (assoc ctx
                                 :recur-arity {:fixed-arity arg-count}) expr)))))

(defn analyze-recur [ctx expr]
  (let [filename (:filename ctx)
        recur-arity (:recur-arity ctx)]
    (when-not (or (linter-disabled? ctx :invalid-arity)
                  (config/skip? (:config ctx) :invalid-arity (:callstack ctx)))
      (let [arg-count (count (rest (:children expr)))
            expected-arity
            (or (:fixed-arity recur-arity)
                ;; varargs must be passed as a seq or nil in recur
                (when-let [min-arity (:min-arity recur-arity)]
                  (inc min-arity)))]
        (cond
          (not expected-arity)
          (findings/reg-finding!
           ctx
           (node->line
            filename
            expr
            :warning
            :unexpected-recur "unexpected recur"))
          (not= expected-arity arg-count)
          (findings/reg-finding!
           ctx
           (node->line
            filename
            expr
            :error
            :invalid-arity
            (format "recur argument count mismatch (expected %d, got %d)" expected-arity arg-count)))
          :else nil))))
  (analyze-children ctx (:children expr)))

(defn analyze-letfn [ctx expr]
  (let [fns (-> expr :children second :children)
        name-exprs (map #(-> % :children first) fns)
        bindings (when (seq name-exprs)
                   (mapv (fn [name-expr]
                           (let [v (cond-> (assoc (meta name-expr)
                                                  :name (:value name-expr)
                                                  :filename (:filename ctx))
                                     (:analyze-locals? ctx)
                                     (-> (assoc :id (swap! (:id-gen ctx) inc)
                                                :str (:str name-expr))
                                         (merge (scope-end expr))))]
                             (namespace/reg-binding! ctx (-> ctx :ns :name) v)
                             [(:value name-expr) v]))
                         name-exprs))
        ctx (ctx-with-bindings ctx bindings)
        processed-fns (for [f fns
                            :let [children (:children f)
                                  fn-name (:value (first children))
                                  bodies (fn-bodies ctx (next children) f)
                                  arity (fn-arity (assoc ctx :skip-reg-binding? true) bodies)]]
                        {:name fn-name
                         :arity arity
                         :bodies bodies})
        ctx (reduce (fn [ctx pf]
                      (assoc-in ctx [:arities (:name pf)]
                                (:arity pf)))
                    ctx processed-fns)
        parsed-fns (map #(analyze-fn-body ctx %) (mapcat :bodies processed-fns))
        analyzed-children (analyze-children ctx (->> expr :children (drop 2)))]
    (concat (mapcat (comp :parsed) parsed-fns) analyzed-children)))

(declare analyze-defmethod)

(defn analyze-def [ctx expr defined-by]
  ;; (def foo ?docstring ?init)
  (let [children (next (:children expr))
        var-name-node (->> children first (meta/lift-meta-content2 ctx))
        metadata (meta var-name-node)
        var-name (:value var-name-node)
        docstring (when (> (count children) 2)
                    (string-from-token (second children)))]
    (when var-name
      (namespace/reg-var! ctx (-> ctx :ns :name)
                          var-name
                          expr
                          (assoc-some metadata
                                      :doc docstring
                                      :output-schema-str (when (:output-schema var-name-node)
                                                           (str (sexpr (:output-schema var-name-node))))
                                      :defined-by defined-by)))
    (analyze-children (assoc ctx
                             :in-def var-name)
                      (nnext (:children expr)))))

(declare analyze-defrecord)

(defn analyze-schema [ctx fn-sym expr defined-by]
  (let [{:keys [:expr :schemas]}
        (schema/expand-schema ctx
                              fn-sym
                              expr)]
    (concat
     (case fn-sym
       fn (analyze-fn ctx expr)
       def (analyze-def ctx expr defined-by)
       defn (analyze-defn ctx expr defined-by)
       defmethod (analyze-defmethod ctx expr)
       defrecord (analyze-defrecord ctx expr defined-by))
     (analyze-children ctx schemas))))

(defn arity-match? [fixed-arities varargs-min-arity arg-count]
  (or (contains? fixed-arities arg-count)
      (and varargs-min-arity (>= arg-count varargs-min-arity))))

(defn analyze-binding-call [ctx fn-name binding expr]
  (let [callstack (:callstack ctx)
        config (:config ctx)
        ns-name (-> ctx :ns :name)
        fn-meta (meta fn-name)]
    (when-let [k (types/keyword binding)]
      (when-not (types/match? k :ifn)
        (findings/reg-finding! ctx (node->line (:filename ctx) expr :error
                                               :type-mismatch
                                               (format "%s cannot be called as a function."
                                                       (str/capitalize (types/label k)))))))
    (namespace/reg-used-binding! ctx
                                 ns-name
                                 binding
                                 (assoc (meta expr)
                                        :name-row (:row fn-meta)
                                        :name-col (:col fn-meta)
                                        :name-end-row (:end-row fn-meta)
                                        :name-end-col (:end-col fn-meta)))
    (when-not (config/skip? config :invalid-arity callstack)
      (let [filename (:filename ctx)
            children (:children expr)]
        (when-not (linter-disabled? ctx :invalid-arity)
          (when-let [{:keys [:fixed-arities :varargs-min-arity]}
                     (get (:arities ctx) fn-name)]
            (let [arg-count (count (rest children))]
              (when-not (arity-match? fixed-arities varargs-min-arity arg-count)
                (findings/reg-finding! ctx
                                       (node->line filename expr :error
                                                   :invalid-arity
                                                   (linters/arity-error nil fn-name arg-count fixed-arities varargs-min-arity)))))))
        (analyze-children (update ctx :callstack conj [nil fn-name]) (rest children))))))

(defn lint-inline-def! [ctx expr]
  (when (:in-def ctx)
    (findings/reg-finding!
     ctx
     (node->line (:filename ctx) expr :warning :inline-def "inline def"))))

(defn analyze-declare [ctx expr defined-by]
  (let [ns-name (-> ctx :ns :name)
        var-name-nodes (next (:children expr))]
    (doseq [var-name-node var-name-nodes]
      (namespace/reg-var! ctx ns-name
                          (->> var-name-node (meta/lift-meta-content2 ctx) :value)
                          expr
                          (assoc (meta expr)
                                 :declared true
                                 :defined-by defined-by)))))

(defn analyze-catch [ctx expr]
  (let [ctx (update ctx :callstack conj [nil 'catch])
        [class-expr binding-expr & exprs] (next (:children expr))
        _ (analyze-expression** ctx class-expr) ;; analyze usage for unused import linter
        binding (extract-bindings ctx binding-expr (last exprs) {})]
    (analyze-children (ctx-with-bindings ctx binding)
                      exprs)))

(defn analyze-try [ctx expr]
  (let [children (next (:children expr))
        children-until-catch-or-finally
        (take-while #(let [sc (symbol-call %)]
                       (and (not= 'catch sc)
                            (not= 'finally sc))) children)
        cnt (count children-until-catch-or-finally)
        children-after (drop cnt children)]
    (analyze-children ctx children-until-catch-or-finally)
    (loop [[fst-child & rst-children] children-after
           analyzed []
           ;; TODO: lint syntax
           _catch-phase false
           _finally-phase false
           has-catch-or-finally? false]
      (if fst-child
        (case (symbol-call fst-child)
          catch
          (let [analyzed-catch (analyze-catch ctx fst-child)]
            (recur rst-children (into analyzed analyzed-catch)
                   true false true))
          finally
          (recur
           rst-children
           (into analyzed (analyze-children (update ctx :callstack conj [nil 'finally])
                                            (next (:children fst-child))))
           false false true)
          ;; TODO: should never get here, probably syntax error
          (recur
           rst-children
           (into analyzed (analyze-expression** ctx fst-child))
           false false has-catch-or-finally?))
        (do
          (when-not has-catch-or-finally?
            (findings/reg-finding!
             ctx
             (node->line
              (:filename ctx)
              expr
              :warning
              :missing-clause-in-try
              "Missing catch or finally in try")))
          analyzed)))))

(defn analyze-defprotocol [{:keys [:ns] :as ctx} expr]
  ;; for syntax, see https://clojure.org/reference/protocols#_basics
  (let [children (next (:children expr))
        name-node (first children)
        protocol-name (:value name-node)
        ns-name (:name ns)
        transduce-arity-vecs (filter
                              ;; skip last docstring
                              #(when (= :vector (tag %)) %))]
    (when protocol-name
      (namespace/reg-var! ctx ns-name protocol-name expr
                          (assoc (meta name-node)
                                 :defined-by 'clojure.core/defprotocol)))
    (doseq [c (next children)
            :when (= :list (tag c)) ;; skip first docstring
            :let [children (:children c)
                  name-node (first children)
                  name-node (meta/lift-meta-content2 ctx name-node)
                  name-meta (meta name-node)
                  fn-name (:value name-node)
                  arities (rest children)]]
      (let [ctx (ctx-with-linter-disabled ctx :unresolved-symbol)]
        (run! #(analyze-usages2 ctx %) arities))
      (when fn-name
        (let [arglist-strs (when (:analyze-arglists? ctx)
                             (->> arities
                                  (into [] (comp transduce-arity-vecs (map str)))
                                  (not-empty)))
              fixed-arities (into #{}
                                  (comp transduce-arity-vecs (map #(count (:children %))))
                                  arities)]
          (namespace/reg-var!
           ctx ns-name fn-name expr
           (assoc-some (meta c)
                       :arglist-strs arglist-strs
                       :name-row (:row name-meta)
                       :name-col (:col name-meta)
                       :name-end-row (:end-row name-meta)
                       :name-end-col (:end-col name-meta)
                       :fixed-arities fixed-arities
                       :defined-by 'clojure.core/defprotocol)))))))

(defn analyze-defrecord
  "Analyzes defrecord, deftype and definterface."
  [{:keys [:ns] :as ctx} expr defined-by]
  (let [ns-name (:name ns)
        children (:children expr)
        children (next children)
        name-node (first children)
        name-node (meta/lift-meta-content2 ctx name-node)
        metadata (meta name-node)
        metadata (assoc metadata :defined-by defined-by)
        record-name (:value name-node)
        bindings? (not= "definterface" (name defined-by))
        binding-vector (when bindings? (second children))
        field-count (when bindings? (count (:children binding-vector)))
        bindings (when bindings? (extract-bindings (assoc ctx
                                                          :skip-reg-binding? true)
                                                   binding-vector
                                                   expr
                                                   {}))
        arglists? (and bindings? (:analyze-arglists? ctx))
        ctx (ctx-with-bindings ctx bindings)]
    (namespace/reg-var! ctx ns-name record-name expr metadata)
    (when-not (= "definterface" (name defined-by))
      (namespace/reg-var! ctx ns-name (symbol (str "->" record-name)) expr
                          (assoc-some metadata
                                      :arglist-strs (when arglists?
                                                      [(str binding-vector)])
                                      :fixed-arities #{field-count})))
    (when (= "defrecord" (name defined-by))
      (namespace/reg-var! ctx ns-name (symbol (str "map->" record-name))
                          expr (assoc-some metadata
                                           :arglist-strs (when arglists?
                                                           ["[m]"])
                                           :fixed-arities #{1})))
    (loop [current-protocol nil
           children (nnext children)]
      (when-first [c children]
        (if-let [sym (utils/symbol-from-token c)]
          ;; we have encountered a protocol or interface name
          (do (analyze-expression** ctx c)
              (recur sym (rest children)))
          ;; Assume protocol fn impl. Analyzing the fn sym can cause false
          ;; positives. We are passing it to analyze-fn as is, so (foo [x y z])
          ;; is linted as (fn [x y z])
          (do (analyze-fn ctx c)
              (recur current-protocol (rest children))))))))

(defn analyze-defmethod [ctx expr]
  (let [children (next (:children expr))
        [method-name-node dispatch-val-node & fn-tail] children
        _ (analyze-usages2 ctx method-name-node)
        _ (analyze-expression** ctx dispatch-val-node)]
    (analyze-fn ctx {:children (cons nil fn-tail)})))

(defn analyze-areduce [ctx expr]
  (let [children (next (:children expr))
        [array-expr index-binding-expr ret-binding-expr init-expr body] children
        index-binding (extract-bindings ctx index-binding-expr expr {})
        ret-binding (extract-bindings ctx ret-binding-expr expr {})
        bindings (merge index-binding ret-binding)
        analyzed-array-expr (analyze-expression** ctx array-expr)
        analyzed-init-expr (analyze-expression** ctx init-expr)
        analyzed-body (analyze-expression** (ctx-with-bindings ctx bindings) body)]
    (concat analyzed-array-expr analyzed-init-expr analyzed-body)))

(defn analyze-this-as [ctx expr]
  (let [[binding-expr & body-exprs] (next (:children expr))
        binding (extract-bindings ctx binding-expr expr {})]
    (analyze-children (ctx-with-bindings ctx binding)
                      body-exprs)))

(defn analyze-as-> [ctx expr]
  (let [children (next (:children expr))
        [as-expr name-expr & forms-exprs] children
        analyzed-as-expr (analyze-expression** ctx as-expr)
        binding (extract-bindings ctx name-expr expr {})]
    (concat analyzed-as-expr
            (analyze-children (ctx-with-bindings ctx binding)
                              forms-exprs))))

(defn analyze-memfn [ctx expr]
  (analyze-children (ctx-with-linter-disabled ctx :unresolved-symbol)
                    (next (:children expr))))

(defn analyze-empty?
  [ctx expr]
  (let [cs (:callstack ctx)
        not-expr (one-of (second cs) [[clojure.core not] [cljs.core not]])]
    (when not-expr
      (findings/reg-finding!
       ctx
       (node->line (:filename ctx) not-expr
                   :warning :not-empty?
                   "use the idiom (seq x) rather than (not (empty? x))")))
    (analyze-children ctx (rest (:children expr)) false)))

(defn analyze-import-libspec [ctx ns-name expr]
  (let [libspec-expr (if (= :quote (tag expr))
                       (first (:children expr))
                       expr)
        analyzed (namespace-analyzer/analyze-import ctx ns-name libspec-expr)]
    (namespace/reg-imports! ctx ns-name analyzed)))

(defn analyze-import
  [ctx expr]
  (let [ns-name (-> ctx :ns :name)
        children (next (:children expr))]
    (run! #(analyze-import-libspec ctx ns-name %) children)))

(defn analyze-if
  "Analyzes if special form for arity errors"
  [ctx expr]
  (let [args (rest (:children expr))]
    (when-let [[expr msg linter]
               (case (count args)
                 (0 1) [expr "Too few arguments to if." :syntax]
                 (2 3) nil
                 [expr "Too many arguments to if." :syntax])]
      (findings/reg-finding!
       ctx
       (node->line (:filename ctx) expr
                   :warning linter
                   msg)))
    (analyze-children ctx args false)))

(defn reg-call [{:keys [:calls-by-id]} call id]
  (swap! calls-by-id assoc id call)
  nil)

(defn analyze-constructor
  "Analyzes (new Foo ...) constructor call."
  [ctx expr]
  (let [[_ ctor-node & children] (:children expr)]
    (analyze-usages2 (ctx-with-linter-disabled ctx :unresolved-symbol) ctor-node)
    (analyze-children ctx children)))

(defn analyze-set!
  [ctx expr]
  (let [children (next (:children expr))]
    (if (and (identical? :cljs (:lang ctx))
             (= 3 (count children)))
      ;; ignore second argument which is the field, e.g. (set! o -x 3)
      (analyze-children ctx (cons (first children) (nnext children)))
      (analyze-children ctx children))))

(defn analyze-with-redefs
  [ctx expr]
  (let [call (-> (:callstack ctx) first second) ;; can be with-redefs or binding
        children (next (:children expr))
        binding-vector (assert-vector ctx call (first children))
        _ (when binding-vector
            (lint-even-forms-bindings! ctx call binding-vector))
        bindings (:children binding-vector)
        lhs (take-nth 2 bindings)
        rhs (take-nth 2 (rest bindings))
        body (next children)]
    ;;  NOTE: because of lazy evaluation we need to use dorun!
    (let [ctx (update ctx :callstack conj [nil :vector])]
      (dorun (analyze-children (ctx-with-linter-disabled ctx :private-call)
                               lhs))
      (dorun (analyze-children ctx rhs)))
    (analyze-children ctx body)))

(defn analyze-def-catch-all [ctx expr]
  (let [ns-name (-> ctx :ns :name)
        children (next (:children expr))
        name-expr (->> (first children)
                       (meta/lift-meta-content2 ctx))
        name-sym (:value name-expr)
        body (next children)
        ctx (ctx-with-linters-disabled ctx [:invalid-arity
                                            :unresolved-symbol
                                            :type-mismatch
                                            :private-call
                                            :missing-docstring])]
    (namespace/reg-var! ctx ns-name name-sym (meta expr))
    (run! #(analyze-usages2 ctx %) body)))

(defn analyze-when [ctx expr]
  (let [children (next (:children expr))
        condition (first children)
        body (next children)]
    (dorun (analyze-expression**
            ;; avoid redundant do check for condition
            (update ctx :callstack conj nil)
            condition))
    (if-not (seq body)
      (findings/reg-finding!
       ctx
       (node->line
        (:filename ctx)
        expr
        :warning
        :missing-body-in-when
        "Missing body in when"))
      (analyze-children ctx body false))))

(defn analyze-clojure-string-replace [ctx expr]
  (let [children (next (:children expr))
        arg-types (:arg-types ctx)]
    (dorun (analyze-children ctx children false))
    (when arg-types
      (let [types @arg-types
            types (rest (map :tag types))
            match-type (types/keyword (first types))
            matcher-type (second types)
            matcher-type (types/keyword matcher-type)]
        (when (and match-type
                   matcher-type
                   (not (identical? matcher-type :any)))
          (case match-type
            :string (when (not (identical? matcher-type :string))
                      (findings/reg-finding!
                       ctx
                       (node->line (:filename ctx) (last children)
                                   :warning :type-mismatch
                                   "String match arg requires string replacement arg.")))
            :char (when (not (identical? matcher-type :char))
                    (findings/reg-finding!
                     ctx
                     (node->line (:filename ctx) (last children)
                                 :warning :type-mismatch
                                 "Char match arg requires char replacement arg.")))
            :regex (when (not (or (identical? matcher-type :string)
                                  ;; we could allow :ifn here, but keywords are
                                  ;; not valid in this position, so we do an
                                  ;; additional check for :map
                                  (identical? matcher-type :fn)
                                  (identical? matcher-type :map)))
                     (findings/reg-finding!
                      ctx
                      (node->line (:filename ctx) (last children)
                                  :warning :type-mismatch
                                  "Regex match arg requires string or function replacement arg.")))
            nil))))))

(defn analyze-proxy-super [ctx expr]
  (let [bindings (:bindings ctx)]
    (when-let [this-binding (get bindings 'this)]
      (let [ns-name (-> ctx :ns :name)]
        (namespace/reg-used-binding! ctx
                                     ns-name
                                     this-binding
                                     nil))))
  (analyze-children ctx (nnext (:children expr)) false))

(defn analyze-amap [ctx expr]
  (let [[_ array idx-binding ret-binding body] (:children expr)
        ctx (ctx-with-bindings ctx
                               (into {} (map #(extract-bindings ctx % expr {})
                                             [idx-binding ret-binding])))]
    (analyze-children ctx [array body] false)))

(defn analyze-format-string [ctx format-str-node format-str args]
  (let [;; we aren't interested in %% or %n
        format-str (str/replace format-str #"%[%n]" "")
        percents (re-seq #"%[^\s%]+" format-str)
        [indexed unindexed]
        (reduce (fn [[indexed unindexed] percent]
                  (if-let [[_ pos] (re-find #"^%(\d+)\$" percent)]
                    [(max indexed (Integer/parseInt pos)) unindexed]
                    [indexed (cond-> unindexed (not= (.charAt ^String percent 1) \<) inc)]))
                [0 0] percents)
        percent-count (max indexed unindexed)
        arg-count (count args)]
    (when-not (= percent-count
                 arg-count)
      (findings/reg-finding! ctx (node->line (:filename ctx) format-str-node :error :format
                                             (format "Format string expects %s arguments instead of %s."
                                                     percent-count arg-count))))))

(defn analyze-format [ctx expr]
  (let [children (next (:children expr))
        format-str-node (first children)
        format-str (utils/string-from-token format-str-node)]
    (when format-str
      (analyze-format-string ctx format-str-node format-str (rest children)))
    (analyze-children ctx children false)))

(defn analyze-formatted-logging [ctx expr]
  (let [children (next (:children expr))]
    (loop [attempt 0
           args (seq children)]
      (when-first [a args]
        (if-let [format-str (utils/string-from-token a)]
          (analyze-format-string ctx a format-str (rest args))
          (when (zero? attempt)
            ;; format string can be either the first or second argument
            (recur (inc attempt) (rest args))))))
    (analyze-children ctx children false)))

(defn analyze-hof [ctx expr resolved-as-name]
  (let [children (next (:children expr))
        f (first children)
        fana (analyze-expression** ctx f)
        fsym (utils/symbol-from-token f)
        binding (get (:bindings ctx) fsym)
        arity (if binding
                (get (:arities ctx) fsym)
                (-> fana meta :arity))
        ns (:ns ctx)
        var? (and fsym (not binding))
        ns-name (:name ns)
        {resolved-namespace :ns
         resolved-name :name
         resolved-alias :alias
         unresolved? :unresolved?
         unresolved-ns :unresolved-ns
         clojure-excluded? :clojure-excluded?
         interop? :interop?
         resolved-core? :resolved-core?
         :as _m} (when var?
                   (resolve-name ctx ns-name fsym))
        var? (and fsym (not binding))
        args (rest children)
        arg-count (cond (one-of resolved-as-name [map mapv mapcat])
                        (count args)
                        (one-of resolved-as-name [reduce map-indexed keep-indexed]) 2
                        :else 1)
        transducer-eligable? (one-of resolved-as-name [map filter remove mapcat map-indexed
                                                       keep keep-indexed])
        arg-count (if (and transducer-eligable?
                           (zero? arg-count)) ;; transducer
                    1
                    arg-count)]
    (cond var?
      (let [{:keys [:row :end-row :col :end-col]} (meta f)]
        (namespace/reg-var-usage! ctx ns-name
                                  {:type :call
                                   :resolved-ns resolved-namespace
                                   :ns ns-name
                                   :name (with-meta
                                           (or resolved-name fsym)
                                           (meta fsym))
                                   :alias resolved-alias
                                   :unresolved? unresolved?
                                   :unresolved-ns unresolved-ns
                                   :clojure-excluded? clojure-excluded?
                                   :arity arg-count
                                   :row row
                                   :end-row end-row
                                   :col col
                                   :end-col end-col
                                   :base-lang (:base-lang ctx)
                                   :lang (:lang ctx)
                                   :filename (:filename ctx)
                                   ;; save some memory during dependencies
                                   :expr (when-not (:dependencies ctx) expr)
                                   :simple? (simple-symbol? fsym)
                                   :callstack (:callstack ctx)
                                   :config (:config ctx)
                                   :top-ns (:top-ns ctx)
                                   ;; :arg-types (:arg-types ctx)
                                   :interop? interop?
                                   :resolved-core? resolved-core?}))
      arity (let [{:keys [:fixed-arities :varargs-min-arity]} arity
                  config (:config ctx)
                  callstack (:callstack ctx)]
              (when-not (config/skip? config :invalid-arity callstack)
                (let [filename (:filename ctx)]
                  (when-not (linter-disabled? ctx :invalid-arity)
                    (when-not (arity-match? fixed-arities varargs-min-arity arg-count)
                      (let [fst-ana (first fana)
                            fn-name (or fsym (:name fst-ana))]
                        (findings/reg-finding!
                         ctx
                         (node->line filename f :error
                                     :invalid-arity
                                     (linters/arity-error nil fn-name arg-count fixed-arities varargs-min-arity))))))))))
    (concat fana
            (analyze-children ctx args false))))

(defn analyze-call
  [{:keys [:top-level? :base-lang :lang :ns :config :dependencies] :as ctx}
   {:keys [:arg-count
           :full-fn-name
           :row :col
           :expr] :as m}]
  (let [ns-name (:name ns)
        children (next (:children expr))
        {resolved-namespace :ns
         resolved-name :name
         resolved-alias :alias
         unresolved? :unresolved?
         unresolved-ns :unresolved-ns
         clojure-excluded? :clojure-excluded?
         interop? :interop?
         resolved-core? :resolved-core?
         :as _m}
        (resolve-name ctx ns-name full-fn-name)
        expr-meta (meta expr)]
    (cond (and unresolved?
               (str/ends-with? full-fn-name "."))
          (recur ctx
                 (let [expr (macroexpand/expand-dot-constructor ctx expr)]
                   (assoc m
                          :expr expr
                          :full-fn-name 'new
                          :arg-count (inc (:arg-count m)))))
          unresolved-ns
          (do
            (namespace/reg-unresolved-namespace! ctx ns-name
                                                 (with-meta unresolved-ns
                                                   (meta full-fn-name)))
            (analyze-children (update ctx :callstack conj [:clj-kondo/unknown-namespace
                                                           (symbol (name full-fn-name))])
                              children))
          :else
          (let [[resolved-as-namespace resolved-as-name _lint-as?]
                (or (when-let
                        [[ns n]
                         (config/lint-as config
                                         [resolved-namespace resolved-name])]
                      [ns n true])
                    [resolved-namespace resolved-name false])
                ;; See #1170, we deliberaly use resolved and not resolved-as
                ;; Users can get :lint-as like behavior for hooks by configuring
                ;; multiple fns to target the same hook code
                hook-fn (hooks/hook-fn ctx config :analyze-call resolved-namespace resolved-name)
                transformed (when hook-fn
                              ;;;; Expand macro using user-provided function
                              (let [filename (:filename ctx)]
                                (binding [hooks/*ctx* ctx]
                                  (sci/binding [sci/out *out*]
                                    (try (hook-fn {:node expr
                                                   :cljc (identical? :cljc base-lang)
                                                   :lang lang
                                                   :filename filename
                                                   :config config})
                                         (catch Exception e
                                           (findings/reg-finding!
                                            ctx
                                            (merge
                                             {:filename filename
                                              :row row
                                              :col col
                                              :type :hook
                                              :message (.getMessage e)}
                                             (select-keys (ex-data e)
                                                          [:level :row :col])))
                                           nil))))))]
            (if-let [expanded (and transformed
                                   (:node transformed))]
              (do ;;;; This registers the macro call, so we still get arity linting
                (namespace/reg-var-usage! ctx ns-name {:type :call
                                                       :resolved-ns resolved-namespace
                                                       :ns ns-name
                                                       :name (with-meta
                                                               (or resolved-name full-fn-name)
                                                               (meta full-fn-name))
                                                       :alias resolved-alias
                                                       :unresolved? unresolved?
                                                       :unresolved-ns unresolved-ns
                                                       :clojure-excluded? clojure-excluded?
                                                       :arity arg-count
                                                       :row row
                                                       :end-row (:end-row expr-meta)
                                                       :col col
                                                       :end-col (:end-col expr-meta)
                                                       :base-lang base-lang
                                                       :lang lang
                                                       :filename (:filename ctx)
                                                       ;; save some memory during dependencies
                                                       :expr (when-not dependencies expr)
                                                       :simple? (simple-symbol? full-fn-name)
                                                       :callstack (:callstack ctx)
                                                       :config (:config ctx)
                                                       :top-ns (:top-ns ctx)
                                                       :arg-types (:arg-types ctx)
                                                       :interop? interop?
                                                       :resolved-core? resolved-core?})
                  ;;;; This registers the namespace as used, to prevent unused warnings
                (namespace/reg-used-namespace! ctx
                                               ns-name
                                               resolved-namespace)
                (let [node expanded]
                  (analyze-expression** (assoc-some ctx :defined-by (:defined-by transformed))
                                        node)))
              ;;;; End macroexpansion
              (let [fq-sym (when (and resolved-namespace
                                      resolved-name)
                             (symbol (str resolved-namespace)
                                     (str resolved-name)))
                    unknown-ns? (= :clj-kondo/unknown-namespace resolved-namespace)
                    resolved-namespace* (if unknown-ns?
                                          ns-name resolved-namespace)
                    ctx (if fq-sym
                          (update ctx :callstack
                                  (fn [cs]
                                    (let [generated? (:clj-kondo.impl/generated expr)]
                                      (cons (with-meta [resolved-namespace* resolved-name]
                                              (cond-> expr-meta
                                                generated? (assoc :clj-kondo.impl/generated true))) cs))))
                          (update ctx :callstack conj [nil nil]))
                    resolved-as-clojure-var-name
                    (when (one-of resolved-as-namespace [clojure.core cljs.core])
                      resolved-as-name)
                    arg-types (if (and resolved-namespace resolved-name
                                       (not (linter-disabled? ctx :type-mismatch)))
                                (atom [])
                                nil)
                    ctx (assoc ctx :arg-types arg-types)
                    ctx (if resolved-as-clojure-var-name
                          (assoc ctx
                                 :resolved-as-clojure-var-name resolved-as-clojure-var-name)
                          ctx)
                    defined-by (or (:defined-by ctx)
                                   (when (and resolved-as-name resolved-as-namespace)
                                     (symbol (name resolved-as-namespace) (name resolved-as-name))))
                    analyzed
                    (case resolved-as-clojure-var-name
                      ns
                      (when top-level?
                        [(analyze-ns-decl ctx expr)])
                      in-ns (if top-level? [(analyze-in-ns ctx expr)]
                                (analyze-children ctx children))
                      alias
                      [(analyze-alias ctx expr)]
                      declare (analyze-declare ctx expr defined-by)
                      (def defonce defmulti goog-define)
                      (do (lint-inline-def! ctx expr)
                          (analyze-def ctx expr defined-by))
                      (defn defn- defmacro definline)
                      (do (lint-inline-def! ctx expr)
                          (analyze-defn ctx expr defined-by))
                      defmethod (analyze-defmethod ctx expr)
                      defprotocol (analyze-defprotocol ctx expr)
                      (defrecord deftype definterface) (analyze-defrecord ctx expr defined-by)
                      comment
                      (let [ctx (assoc ctx :in-comment true)]
                        (analyze-children ctx children))
                      (-> some->)
                      (analyze-expression** ctx (macroexpand/expand-> ctx expr))
                      (->> some->>)
                      (analyze-expression** ctx (macroexpand/expand->> ctx expr))
                      doto
                      (analyze-expression** ctx (macroexpand/expand-doto ctx expr))
                      (. .. proxy extend-protocol reify
                         defcurried extend-type)
                      ;; don't lint calls in these expressions, only register them as used vars
                      (analyze-children (ctx-with-linters-disabled ctx [:invalid-arity
                                                                        :unresolved-symbol
                                                                        :type-mismatch])
                                        children)
                      (proxy-super)
                      (analyze-proxy-super ctx expr)
                      (amap)
                      (analyze-amap ctx expr)
                      (cond-> cond->>)
                      (analyze-expression** ctx (macroexpand/expand-cond->
                                                 ctx expr
                                                 resolved-as-name))
                      (let let* for doseq dotimes with-open with-local-vars)
                      (analyze-like-let ctx expr)
                      letfn
                      (analyze-letfn ctx expr)
                      (if-let if-some when-let when-some when-first)
                      (analyze-conditional-let ctx resolved-as-clojure-var-name expr)
                      do
                      (analyze-do ctx expr)
                      (fn fn* bound-fn)
                      (analyze-fn ctx expr)
                      case
                      (analyze-case ctx expr)
                      loop
                      (analyze-loop ctx expr)
                      recur
                      (analyze-recur ctx expr)
                      quote nil
                      try (analyze-try ctx expr)
                      as-> (analyze-as-> ctx expr)
                      areduce (analyze-areduce ctx expr)
                      this-as (analyze-this-as ctx expr)
                      memfn (analyze-memfn ctx expr)
                      empty? (analyze-empty? ctx expr)
                      format (analyze-format ctx expr)
                      (use require)
                      (if top-level? (namespace-analyzer/analyze-require ctx expr)
                          (analyze-children ctx children))
                      import
                      (if top-level? (analyze-import ctx expr)
                          (analyze-children ctx children))
                      if (analyze-if ctx expr)
                      new (analyze-constructor ctx expr)
                      set! (analyze-set! ctx expr)
                      (with-redefs binding) (analyze-with-redefs ctx expr)
                      (when when-not) (analyze-when ctx expr)
                      (map mapv filter filterv remove reduce
                           every? not-every? some not-any? mapcat iterate
                           max-key min-key group-by partition-by map-indexed
                           keep keep-indexed)
                      (analyze-hof ctx expr resolved-as-name)
                      ;; catch-all
                      (case [resolved-as-namespace resolved-as-name]
                        [clj-kondo.lint-as def-catch-all]
                        (analyze-def-catch-all ctx expr)
                        [schema.core fn]
                        (analyze-schema ctx 'fn expr 'schema.core/fn)
                        [schema.core def]
                        (analyze-schema ctx 'def expr 'schema.core/def)
                        [schema.core defn]
                        (analyze-schema ctx 'defn expr 'schema.core/defn)
                        [schema.core defmethod]
                        (analyze-schema ctx 'defmethod expr 'schema.core/defmethod)
                        [schema.core defrecord]
                        (analyze-schema ctx 'defrecord expr 'schema.core/defrecord)
                        ([clojure.test deftest]
                         [cljs.test deftest])
                        (test/analyze-deftest ctx expr defined-by
                                              resolved-as-namespace resolved-as-name)
                        ([clojure.core.match match] [cljs.core.match match])
                        (match/analyze-match ctx expr)
                        [clojure.string replace]
                        (analyze-clojure-string-replace ctx expr)
                        [cljs.test async]
                        (test/analyze-cljs-test-async ctx expr)
                        ([clojure.test are] [cljs.test are])
                        (test/analyze-are ctx resolved-namespace expr)
                        [cljs.spec.alpha def]
                        (spec/analyze-def ctx expr 'cljs.spec.alpha/def)
                        [clojure.spec.alpha def]
                        (spec/analyze-def ctx expr 'clojure.spec.alpha/def)
                        ([clojure.spec.alpha fdef] [cljs.spec.alpha fdef])
                        (spec/analyze-fdef (assoc ctx
                                                  :analyze-children
                                                  analyze-children) expr)
                        ([clojure.spec.alpha keys] [cljs.spec.alpha keys])
                        (spec/analyze-keys ctx expr)
                        ([clojure.spec.gen.alpha lazy-combinators]
                         [clojure.spec.gen.alpha lazy-prims])
                        (analyze-declare ctx expr defined-by)
                        [potemkin import-vars]
                        (potemkin/analyze-import-vars ctx expr)
                        ([clojure.core.async alt!] [clojure.core.async alt!!]
                         [cljs.core.async alt!] [cljs.core.async alt!!])
                        (core-async/analyze-alt! (assoc ctx
                                                        :analyze-expression** analyze-expression**
                                                        :extract-bindings extract-bindings)
                                                 expr)
                        ([clojure.core.async defblockingop])
                        (analyze-defn ctx expr defined-by)
                        ([clojure.core.reducers defcurried])
                        (analyze-defn ctx expr defined-by)
                        ([clojure.template do-template])
                        (analyze-expression** ctx (macroexpand/expand-do-template ctx expr))
                        ([datahike.api q]
                         [datascript.core q]
                         [datomic.api q]
                         [datomic.client.api q])
                        (do (datalog/analyze-datalog ctx expr)
                            (analyze-children ctx children false))
                        ([compojure.core GET]
                         [compojure.core POST]
                         [compojure.core PUT]
                         [compojure.core DELETE]
                         [compojure.core HEAD]
                         [compojure.core OPTIONS]
                         [compojure.core PATCH]
                         [compojure.core ANY]
                         [compojure.core context]
                         [compojure.core rfn])
                        (compojure/analyze-compojure-macro ctx expr resolved-as-name)
                        ([clojure.java.jdbc with-db-transaction]
                         [clojure.java.jdbc with-db-connection]
                         [clojure.java.jdbc with-db-metadata]
                         [next.jdbc with-transaction])
                        (jdbc/analyze-like-jdbc-with ctx expr)
                        ([clojure.tools.logging debugf]
                         [clojure.tools.logging infof]
                         [clojure.tools.logging errorf]
                         [clojure.tools.logging logf]
                         [clojure.tools.logging spyf]
                         [clojure.tools.logging tracef]
                         [clojure.tools.logging warnf])
                        (analyze-formatted-logging ctx expr)
                        [clojure.data.xml alias-uri]
                        (xml/analyze-alias-uri ctx expr)
                        [clojure.data.xml.impl export-api]
                        (xml/analyze-export-api ctx expr)
                        [cljs.core simple-benchmark]
                        (analyze-like-let ctx expr)
                        [babashka.process $]
                        (babashka/analyze-$ ctx expr)
                        ([re-frame.core reg-event-db]
                         [re-frame.core reg-event-fx]
                         [re-frame.core reg-event-ctx]
                         [re-frame.core reg-sub]
                         [re-frame.core reg-sub-raw]
                         [re-frame.core reg-fx]
                         [re-frame.core reg-cofx])
                        (re-frame/analyze-reg ctx expr (symbol (str resolved-namespace) (str resolved-name)))
                        ;; catch-all
                        (let [next-ctx (cond-> ctx
                                         (one-of [resolved-namespace resolved-name]
                                                 [[clojure.core.async thread]
                                                  [clojure.core dosync]
                                                  [clojure.core lazy-seq]
                                                  [clojure.core lazy-cat]])
                                         (assoc-in [:recur-arity :fixed-arity] 0))]
                          (analyze-children next-ctx children false))))]
                (if (= 'ns resolved-as-clojure-var-name)
                  analyzed
                  (let [in-def (:in-def ctx)
                        id (:id expr)
                        m (meta analyzed)
                        proto-call {:type :call
                                    :resolved-ns resolved-namespace
                                    :ns ns-name
                                    :name (with-meta
                                            (or resolved-name full-fn-name)
                                            (meta full-fn-name))
                                    :alias resolved-alias
                                    :unresolved? unresolved?
                                    :unresolved-ns unresolved-ns
                                    :clojure-excluded? clojure-excluded?
                                    :arity arg-count
                                    :row row
                                    :end-row (:end-row expr-meta)
                                    :col col
                                    :end-col (:end-col expr-meta)
                                    :base-lang base-lang
                                    :lang lang
                                    :filename (:filename ctx)
                                    :expr (when-not dependencies expr)
                                    :callstack (:callstack ctx)
                                    :config (:config ctx)
                                    :top-ns (:top-ns ctx)
                                    :arg-types (:arg-types ctx)
                                    :simple? (simple-symbol? full-fn-name)
                                    :interop? interop?
                                    :resolved-core? resolved-core?}
                        ret-tag (or (:ret m)
                                    (types/ret-tag-from-call ctx proto-call expr))
                        call (cond-> proto-call
                               id (assoc :id id)
                               in-def (assoc :in-def in-def)
                               ret-tag (assoc :ret ret-tag))]
                    (when id (reg-call ctx call id))
                    (namespace/reg-var-usage! ctx ns-name call)
                    (when-not unresolved?
                      (namespace/reg-used-namespace! ctx
                                                     ns-name
                                                     resolved-namespace))
                    (if m
                      (with-meta (cons call analyzed)
                        m)
                      (cons call analyzed))))))))))

(defn lint-keyword-call! [ctx kw namespaced? arg-count expr]
  (let [callstack (:callstack ctx)
        config (:config ctx)]
    (when-not (config/skip? config :invalid-arity callstack)
      (let [ns (:ns ctx)
            ?resolved-ns (if namespaced?
                           (if-let [kw-ns (namespace kw)]
                             (or (get (:qualify-ns ns) (symbol kw-ns))
                                 ;; because we couldn't resolve the namespaced
                                 ;; keyword, we print it as is
                                 (str ":" (namespace kw)))
                             ;; if the keyword is namespace, but there is no
                             ;; namespace, it's the current ns
                             (:name ns))
                           (namespace kw))
            kw-str (if ?resolved-ns (str ?resolved-ns "/" (name kw))
                       (str (name kw)))]
        (when (or (zero? arg-count)
                  (> arg-count 2))
          (findings/reg-finding! ctx
                                 (node->line (:filename ctx) expr :error :invalid-arity
                                             (format "keyword :%s is called with %s args but expects 1 or 2"
                                                     kw-str
                                                     arg-count))))))))

(defn lint-map-call! [ctx _the-map arg-count expr]
  (let [callstack (:callstack ctx)
        config (:config ctx)]
    (when (or (zero? arg-count)
              (> arg-count 2))
      (when-not (config/skip? config :invalid-arity callstack)
        (findings/reg-finding!
         ctx
         (node->line (:filename ctx) expr :error :invalid-arity
                     (format "map is called with %s args but expects 1 or 2"
                             arg-count)))))))

(defn lint-vector-call! [ctx _the-map arg-count expr]
  (let [callstack (:callstack ctx)
        config (:config ctx)]
    (when (not= 1 arg-count)
      (when-not (config/skip? config :invalid-arity callstack)
        (findings/reg-finding!
         ctx
         (node->line (:filename ctx) expr :error :invalid-arity
                     (str "Vector can only be called with 1 arg but was called with: "
                          arg-count)))))))

(defn lint-symbol-call! [ctx _the-symbol arg-count expr]
  (let [callstack (:callstack ctx)
        config (:config ctx)]
    (when (or (zero? arg-count)
              (> arg-count 2))
      (when-not (config/skip? config :invalid-arity callstack)
        (findings/reg-finding!
         ctx
         (node->line (:filename ctx) expr :error :invalid-arity
                     (format "symbol is called with %s args but expects 1 or 2"
                             arg-count)))))))

;; TODO: this should just be a case of :type-mismatch
(defn reg-not-a-function! [ctx expr typ]
  (let [callstack (:callstack ctx)
        config (:config ctx)]
    (when-not (config/skip? config :not-a-function callstack)
      (findings/reg-finding!
       ctx
       (node->line (:filename ctx) expr :error :not-a-function (str "a " typ " is not a function"))))))

(defn analyze-reader-macro [ctx expr]
  (analyze-children ctx (rest (:children expr))))

(defn analyze-expression**
  [{:keys [:bindings :lang] :as ctx}
   {:keys [:children] :as expr}]
  (when expr
    (let [expr (if (or (not= :edn lang)
                       (:quoted ctx))
                 (meta/lift-meta-content2 (dissoc ctx :arg-types) expr)
                 expr)
          t (tag expr)
          {:keys [:row :col]} (meta expr)
          arg-count (count (rest children))]
      (utils/handle-ignore ctx expr)
      (when-not (one-of t [:map :list :quote]) ;; list and quote are handled specially because of return types
        (types/add-arg-type-from-expr ctx expr))
      (case t
        :quote (let [ctx (assoc ctx :quoted true)]
                 (types/add-arg-type-from-expr ctx (first (:children expr)))
                 (analyze-children ctx children))
        :syntax-quote (analyze-usages2 (assoc ctx :arg-types nil) expr)
        :var (analyze-children (assoc ctx :private-access? true)
                               (:children expr))
        :reader-macro (analyze-reader-macro ctx expr)
        (:unquote :unquote-splicing)
        (analyze-children ctx children)
        :namespaced-map (usages/analyze-namespaced-map
                         (-> ctx
                             (assoc :analyze-expression**
                                    analyze-expression**)
                             (update :callstack #(cons [nil t] %)))
                         expr)
        :map (do (key-linter/lint-map-keys ctx expr)
                 (let [children (map (fn [c s]
                                       (assoc c :id s))
                                     children
                                     (repeatedly #(gensym)))
                       analyzed (analyze-children
                                 (update ctx
                                         :callstack #(cons [nil t] %)) children)]
                   (types/add-arg-type-from-expr ctx (assoc expr
                                                            :children children
                                                            :analyzed analyzed))
                   analyzed))
        :set (do (key-linter/lint-set ctx expr)
                 (analyze-children (update ctx
                                           :callstack #(cons [nil t] %))
                                   children))
        :fn (do
              (when (and (:in-fn-literal ctx)
                         (not (:clj-kondo.impl/generated expr)))
                (findings/reg-finding! ctx (assoc (meta expr)
                                                  :filename (:filename ctx)
                                                  :level :error
                                                  :type :syntax
                                                  :message "Nested #()s are not allowed")))
              (recur (assoc ctx :arg-types nil :in-fn-literal true)
                     (macroexpand/expand-fn expr)))
        :token
        (if (:quoted ctx)
          (when (:k expr)
            (usages/analyze-keyword ctx expr))
          (when-not (= :edn (:lang ctx))
            (analyze-usages2 ctx expr)))
        :list
        (if-let [function (some->>
                           (first children)
                           (meta/lift-meta-content2 (dissoc ctx :arg-types)))]
          (if (or (:quoted ctx) (= :edn (:lang ctx)))
            (analyze-children ctx children)
            (let [t (tag function)]
              (case t
                :map
                (do (lint-map-call! ctx function arg-count expr)
                    (types/add-arg-type-from-expr ctx expr)
                    (analyze-children (update ctx :callstack conj [nil t]) children))
                :quote
                (let [quoted-child (-> function :children first)]
                  (if (utils/symbol-token? quoted-child)
                    (do (lint-symbol-call! ctx quoted-child arg-count expr)
                        (analyze-children (update ctx :callstack conj [nil t])
                                          children))
                    (do (types/add-arg-type-from-expr ctx expr)
                        (analyze-children ctx
                                          (update ctx :callstack conj [nil t])
                                          children))))
                :vector
                (do (lint-vector-call! ctx function arg-count expr)
                    (types/add-arg-type-from-expr ctx expr)
                    (analyze-children (update ctx :callstack conj [nil t]) children))
                :token
                (if-let [k (:k function)]
                  (do
                    (lint-keyword-call! ctx k (:namespaced? function) arg-count expr)
                    (types/add-arg-type-from-expr ctx expr)
                    (analyze-children (update ctx :callstack conj [nil t]) children))
                  (if-let [full-fn-name (let [s (utils/symbol-from-token function)]
                                          (when-not (one-of s ['. '..])
                                            s))]
                    (let [simple? (simple-symbol? full-fn-name)
                          full-fn-name (if simple?
                                         (namespace/normalize-sym-name ctx full-fn-name)
                                         full-fn-name)
                          full-fn-name (with-meta full-fn-name (meta function))
                          binding (and simple?
                                       (get bindings full-fn-name))]
                      (if binding
                        (do
                          (types/add-arg-type-from-expr ctx expr)
                          (analyze-binding-call ctx full-fn-name binding expr))
                        (let [ret (analyze-call ctx {:arg-count arg-count
                                                     :full-fn-name full-fn-name
                                                     :row row
                                                     :col col
                                                     :expr expr})
                              maybe-call (first ret)]
                          (if (identical? :call (:type maybe-call))
                            (types/add-arg-type-from-call ctx maybe-call expr)
                            (types/add-arg-type-from-expr ctx expr))
                          ret)))
                    (cond
                      (utils/boolean-token? function)
                      (do (reg-not-a-function! ctx expr "boolean")
                          (analyze-children (update ctx :callstack conj [nil t])
                                            (rest children)))
                      (utils/string-from-token function)
                      (do (reg-not-a-function! ctx expr "string")
                          (analyze-children (update ctx :callstack conj [nil t])
                                            (rest children)))
                      (utils/char-token? function)
                      (do (reg-not-a-function! ctx expr "character")
                          (analyze-children (update ctx :callstack conj [nil t])
                                            (rest children)))
                      (utils/number-token? function)
                      (do (reg-not-a-function! ctx expr "number")
                          (analyze-children (update ctx :callstack conj [nil t])
                                            (rest children)))
                      :else
                      (do
                        (types/add-arg-type-from-expr (update ctx :callstack conj [nil t]) expr)
                        (analyze-children ctx children)))))
                ;; catch-all
                (do
                  ;; (prn "--" expr (types/add-arg-type-from-expr ctx expr))
                  (types/add-arg-type-from-expr ctx expr)
                  (let [ctx (update ctx :callstack conj [nil t])]
                    (analyze-children ctx children))))))
          (types/add-arg-type-from-expr ctx expr :list))
        ;; catch-all
        (analyze-children (update ctx
                                  :callstack #(cons [nil t] %))
                          children)))))

;; Hack to make a few functions available in a common namespace without
;; introducing circular depending namespaces. NOTE: alter-var-root! didn't work
;; with GraalVM
(vreset! common {'analyze-expression** analyze-expression**
                 'analyze-children analyze-children
                 'analyze-like-let analyze-like-let
                 'ctx-with-bindings ctx-with-bindings
                 'extract-bindings extract-bindings
                 'analyze-defn analyze-defn
                 'analyze-usages2 analyze-usages2})

(defn analyze-expression*
  "NOTE: :used-namespaces is used in the cache to load namespaces that were actually used."
  [ctx expression]
  (loop [ctx (assoc ctx
                    :bindings {}
                    :top-level? true)
         ns (:ns ctx)
         [first-parsed & rest-parsed :as all]
         (analyze-expression** ctx expression)]
    (if (seq all)
      (case (:type first-parsed)
        nil (recur ctx ns rest-parsed)
        (:ns :in-ns)
        (let [ns-name (:name first-parsed)
              local-config (:config first-parsed)
              global-config (:global-config ctx)
              new-config (config/merge-config! global-config local-config)]
          (swap! (:used-namespaces ctx) update (:base-lang ctx) into (:used-namespaces first-parsed))
          (recur
           (-> ctx
               (assoc :config new-config)
               (update :top-ns (fn [n]
                                 (or n ns-name))))
           first-parsed
           rest-parsed))
        :import-vars
        (do
          (namespace/reg-proxied-namespaces! ctx (:name ns) (:used-namespaces first-parsed))
          (swap! (:used-namespaces ctx) update (:base-lang ctx) into (:used-namespaces first-parsed))
          (recur ctx
                 ns
                 rest-parsed))
        ;; catch-all
        (do (swap! (:used-namespaces ctx) update (:base-lang ctx) conj (:resolved-ns first-parsed))
            (recur
             ctx
             ns
             rest-parsed)))
      (assoc ctx :ns ns))))

(defn analyze-expressions
  "Analyzes expressions and collects defs and calls into a map. To
  optimize cache lookups later on, calls are indexed by the namespace
  they call to, not the ns where the call occurred."
  [{:keys [:base-lang :lang :config] :as ctx}
   expressions]
  (let [init-ns (when-not (= :edn lang)
                  (analyze-ns-decl (assoc-in ctx
                                             [:config :output :analysis] false)
                                   (parse-string "(ns user)")))
        init-ctx (assoc ctx
                        :ns init-ns
                        :calls-by-id (atom {})
                        :top-ns nil
                        :global-config config)]
    (swap! (:used-namespaces ctx)
           update base-lang into (:used-namespaces init-ns))
    (loop [ctx init-ctx
           [expression & rest-expressions] expressions]
      (if expression
        (let [ctx (analyze-expression* ctx expression)]
          (recur ctx rest-expressions))
        nil))))

;;;; processing of string input

(defn- ->findings
  "Convert an exception thrown from rewrite-clj into a sequence clj-kondo :finding"
  [^Exception ex ^String filename]
  (let [{:keys [findings line col] :as d} (ex-data ex)]
    (cond
      (seq findings)
      (for [finding findings]
        (merge {:type :syntax
                :filename filename}
               finding))

      ;; The edn parser in tools.reader throw ex-data with the following shape:
      ;; {:type :reader-exception, :ex-kind :reader-error, :file nil, :line 1, :col 4}
      (identical? :reader-exception (:type d))
      [{:type :syntax
        :filename filename
        :row line
        :col col
        :message (.getMessage ex)}]

      :else
      [{:filename filename
        :col 0
        :row 0
        :type :syntax
        :message (str "Can't parse "
                      filename ", "
                      (or (.getMessage ex) (str ex)))}])))

(defn analyze-input
  "Analyzes input and returns analyzed defs, calls. Also invokes some
  linters and returns their findings."
  [{:keys [:config] :as ctx} filename input lang dev?]
  (try
    (let [token-exceptions (atom [])
          parsed (binding [*invalid-token-exceptions* token-exceptions]
                   (p/parse-string input))]
      (doseq [e @token-exceptions]
        (if dev?
          (throw e)
          (run! #(findings/reg-finding! ctx %) (->findings e filename))))
      (case lang
        :cljc
        (let [cljc-config (:cljc config)
              features (or (:features cljc-config)
                           [:clj :cljs])]
          (doseq [lang features]
            (analyze-expressions (assoc ctx :base-lang :cljc :lang lang :filename filename)
                                 (:children (select-lang parsed lang)))))
        (:clj :cljs :edn)
        (let [ctx (assoc ctx :base-lang lang :lang lang :filename filename)]
          (analyze-expressions ctx (:children parsed))
          ;; analyze-expressions should go first in order to process ignores
          (when (identical? :edn lang)
            (let [fn (.getName (io/file filename))]
              (when (= fn "deps.edn")
                (deps-edn/lint-deps-edn ctx (first (:children parsed)))))))))
    (catch Exception e
      (if dev?
        (throw e)
        (run! #(findings/reg-finding! ctx %) (->findings e filename))))
    (finally
      (let [output-cfg (:output config)]
        (when (and (= :text (:format output-cfg))
                   (:progress output-cfg))
          (print ".") (flush))))))

;;;; Scratch

(comment
  (parse-string "#js [1 2 3]"))
