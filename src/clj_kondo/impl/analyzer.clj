(ns clj-kondo.impl.analyzer
  {:no-doc true}
  (:require
   [clj-kondo.impl.config :as config]
   [clj-kondo.impl.linters :as l]
   [clj-kondo.impl.namespace :as namespace :refer [analyze-ns-decl resolve-name]]
   [clj-kondo.impl.parser :as p]
   [clj-kondo.impl.utils :refer [some-call call node->line
                                 parse-string parse-string-all
                                 tag select-lang vconj deep-merge]]
   [clj-kondo.impl.metadata :refer [lift-meta]]
   [clj-kondo.impl.macroexpand :as macroexpand]
   [clj-kondo.impl.linters.keys :as key-linter]
   [clojure.set :as set]
   [clojure.string :as str]
   [rewrite-clj.node.protocols :as node]
   [clj-kondo.impl.schema :as schema]
   [clj-kondo.impl.profiler :as profiler]
   [clj-kondo.impl.var-info :as var-info]
   [clj-kondo.impl.state :as state]))

(defn extract-bindings [sexpr]
  (cond (and (symbol? sexpr)
             (not= '& sexpr)) [sexpr]
        (vector? sexpr) (mapcat extract-bindings sexpr)
        (map? sexpr)
        (mapcat extract-bindings
                (for [[k v] sexpr
                      :let [bindings
                            (cond (keyword? k)
                                  (case (keyword (name k))
                                    (:keys :syms :strs)
                                    (map #(-> % name symbol) v)
                                    :or (extract-bindings v)
                                    :as [v])
                                  (symbol? k) [k]
                                  :else nil)]
                      b bindings]
                  b))
        :else []))

(defn analyze-in-ns [ctx {:keys [:children] :as expr}]
  (let [ns-name (-> children second :children first :value)]
    {:type :in-ns
     :name ns-name
     :lang (:lang ctx)}))

(defn fn-call? [expr]
  (let [tag (node/tag expr)]
    (and (= :list tag)
         (symbol? (:value (first (:children expr)))))))

;;;; function arity

(declare analyze-expression**)

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

(defn analyze-children [{:keys [:parents] :as ctx} children]
  (when-not (config/skip? parents)
    (mapcat #(analyze-expression** ctx %) children)))

(defn analyze-fn-body [{:keys [bindings] :as ctx} body]
  (let [children (:children body)
        arg-vec  (first children)
        body-exprs (rest children)
        arg-list (node/sexpr arg-vec)
        arg-bindings (extract-bindings arg-list)
        arity (analyze-arity arg-list)
        parsed (analyze-children (assoc ctx
                                        :bindings (set/union bindings (set arg-bindings))
                                        :recur-arity arity
                                        :fn-body true) body-exprs)]
    (assoc arity
           :parsed
           parsed)))

(defn analyze-defn [{:keys [filename lang ns bindings] :as ctx} expr]
  (let [children (:children expr)
        children (rest children) ;; "my-fn docstring" {:no-doc true} [x y z] x
        name-node (first children)
        fn-name (:value name-node)
        var-meta (meta name-node)
        macro? (or (= 'defmacro (call expr))
                   (:macro var-meta))
        private? (or (= 'defn- (call expr))
                     (:private var-meta))
        bodies (loop [i 0 [expr & rest-exprs :as exprs] (next children)]
                 (let [t (when expr (node/tag expr))]
                   (cond (= :vector t)
                         [{:children exprs}]
                         (= :list t)
                         exprs
                         (not t) []
                         :else (recur (inc i) rest-exprs))))
        parsed-bodies (map #(analyze-fn-body ctx %) bodies)
        fixed-arities (set (keep :fixed-arity parsed-bodies))
        var-args-min-arity (:min-arity (first (filter :varargs? parsed-bodies)))
        {:keys [:row :col]} (meta expr)
        defn
        (if (and fn-name (seq parsed-bodies))
          (cond-> {:type :defn
                   :name fn-name
                   :row row
                   :col col
                   :lang lang}
            macro? (assoc :macro true)
            (seq fixed-arities) (assoc :fixed-arities fixed-arities)
            private? (assoc :private? private?)
            var-args-min-arity (assoc :var-args-min-arity var-args-min-arity))
          {:type :debug
           :level :info
           :message "Could not parse defn form"
           :row row
           :col col
           :lang lang})]
    (cons defn (mapcat :parsed parsed-bodies))))

(defn analyze-case [ctx expr]
  (let [exprs (-> expr :children)]
    (loop [[constant expr :as exprs] exprs
           parsed []]
      (if-not expr
        (into parsed (when constant
                       (analyze-expression** ctx constant)))
        (recur
         (nnext exprs)
         (into parsed (analyze-expression** ctx expr)))))))

(defn expr-bindings [binding-vector]
  (->> binding-vector :children
       (take-nth 2)
       (map node/sexpr)
       (mapcat extract-bindings) set))

(defn lint-even-forms-bindings [form-name expr sexpr]
  (let [num-children (count sexpr)
        {:keys [:row :col]} (meta expr)]
    (when (odd? num-children)
      {:type :invalid-bindings
       :message (format "%s binding vector requires even number of forms" form-name)
       :row row
       :col col
       :level :error})))

(defn analyze-let [{:keys [bindings] :as ctx} expr]
  (let [bv (-> expr :children second)
        bs (expr-bindings bv)]
    (cons
     (lint-even-forms-bindings 'let bv (node/sexpr bv))
     (analyze-children (assoc ctx :bindings (set/union bindings bs))
                       (rest (:children expr))))))

(defn lint-two-forms-binding-vector [form-name expr sexpr]
  (let [num-children (count sexpr)
        {:keys [:row :col]} (meta expr)]
    (when (not= 2 num-children)
      {:type :invalid-bindings
       :message (format "%s binding vector requires exactly 2 forms" form-name)
       :row row
       :col col
       :level :error})))

(defn analyze-if-let [{:keys [bindings] :as ctx} expr]
  (let [bv (-> expr :children second)
        bs (expr-bindings bv)
        sexpr (node/sexpr bv)]
    (list* (lint-two-forms-binding-vector 'if-let bv sexpr)
           (analyze-children (assoc ctx :bindings
                                    (set/union bindings bs))
                             (rest (:children expr))))))

(defn analyze-when-let [{:keys [bindings] :as ctx} expr]
  (let [bv (-> expr :children second)
        bs (expr-bindings bv)
        sexpr (node/sexpr bv)]
    (list* (lint-two-forms-binding-vector 'when-let bv sexpr)
           (analyze-children (assoc ctx :bindings
                                    (set/union bindings bs))
                             (rest (:children expr))))))

(defn analyze-fn [ctx expr]
  (let [children (:children expr)
        ?fn-name (let [n (node/sexpr (second children))]
                   (when (symbol? n) n))
        bodies (loop [i 0 [expr & rest-exprs :as exprs] (next children)]
                 (let [t (when expr (node/tag expr))]
                   (cond (= :vector t)
                         [{:children exprs}]
                         (= :list t)
                         exprs
                         (not t) []
                         :else (recur (inc i) rest-exprs))))
        parsed-bodies (map #(analyze-fn-body (if ?fn-name
                                               (update ctx :bindings conj ?fn-name)
                                               ctx) %) bodies)]
    (mapcat :parsed parsed-bodies)))

(defn analyze-alias [ns expr]
  (let [[alias-sym ns-sym]
        (map #(-> % :children first :value)
             (rest (:children expr)))]
    (assoc-in ns [:qualify-ns alias-sym] ns-sym)))

(defn analyze-loop [{:keys [:bindings] :as ctx} expr]
  (let [bv (-> expr :children second)
        arg-count (let [c (count (:children bv))]
                    (when (even? c)
                      (/ c 2)))
        bs (expr-bindings bv)]
    (cons
     (lint-even-forms-bindings 'loop bv (node/sexpr bv))
     (analyze-children (assoc ctx
                              :bindings (set/union bindings bs)
                              :recur-arity {:fixed-arity arg-count})
                       (rest (:children expr))))))

(defn analyze-recur [ctx expr]
  (let [arg-count (count (rest (:children expr)))
        recur-arity (-> ctx :recur-arity)
        expected-arity
        (or (:fixed-arity recur-arity)
            ;; var-args must be passed as a seq or nil in recur
            (when-let [min-arity (:min-arity recur-arity)]
              (inc min-arity)))]
    (cond
      (not expected-arity)
      (state/reg-finding! (node->line
                           (:filename ctx)
                           expr
                           :warning
                           :unexpected-recur "unexpected recur"))
      (not= expected-arity arg-count)
      (state/reg-finding!
       (node->line
        (:filename ctx)
        expr
        :error
        :invalid-arity
        (format "recur argument count mismatch (expected %d, got %d)" expected-arity arg-count)))
      :else nil)))

(defn analyze-expression**
  [{:keys [filename lang ns bindings fn-body parents] :as ctx}
   {:keys [:children] :as expr}]
  (let [t (node/tag expr)
        {:keys [:row :col]} (meta expr)
        arg-count (count (rest children))]
    (case t
      (:quote :syntax-quote) []
      :map (do (key-linter/lint-map-keys filename expr)
               (analyze-children ctx children))
      :set (do (key-linter/lint-set filename expr)
               (analyze-children ctx children))
      :fn (recur ctx (macroexpand/expand-fn expr))
      (let [?full-fn-name (call expr)
            {resolved-namespace :ns
             resolved-name :name}
            (when ?full-fn-name (resolve-name ns ?full-fn-name))
            [resolved-namespace resolved-name]
            (config/treat-as [resolved-namespace resolved-name])
            fq-sym (when (and resolved-namespace
                              resolved-name)
                     (symbol (str resolved-namespace)
                             (str resolved-name)))
            next-ctx (if fq-sym
                       (update ctx :parents
                               vconj
                               [resolved-namespace resolved-name])
                       ctx)
            resolved-clojure-var-name
            (when (contains? '#{clojure.core
                                cljs.core}
                             resolved-namespace)
              resolved-name)]
        (case resolved-clojure-var-name
          ns
          (let [ns (analyze-ns-decl lang expr)]
            [ns])
          in-ns (when-not fn-body [(analyze-in-ns {:lang lang} expr)])
          alias
          [(analyze-alias ns expr)]
          (defn defn- defmacro)
          (cons {:type :call
                 :name 'defn
                 :row row
                 :col col
                 :lang lang
                 :expr expr
                 :arity arg-count}
                (analyze-defn ctx (lift-meta filename expr)))
          comment
          (analyze-children next-ctx children)
          ->
          (recur ctx (macroexpand/expand-> filename expr))
          ->>
          (recur ctx (macroexpand/expand->> filename expr))
          (cond-> cond->> some-> some->> . .. deftype
                  proxy extend-protocol doto reify definterface defrecord defprotocol
                  defcurried letfn)
          []
          let
          (analyze-let ctx expr)
          if-let
          (analyze-if-let ctx expr)
          when-let
          (analyze-when-let ctx expr)
          fn
          (analyze-fn ctx (lift-meta filename expr))
          case
          (analyze-case ctx expr)
          loop
          (analyze-loop ctx expr)
          recur
          (analyze-recur ctx expr)
          ;; catch-all
          (case [resolved-namespace resolved-name]
            [schema.core defn]
            (cons {:type :call
                   :name 'schema.core/defn
                   :row row
                   :col col
                   :lang lang
                   :expr expr
                   :arity arg-count}
                  (analyze-defn ctx (schema/expand-schema-defn
                                     (lift-meta filename expr))))
            ;;[cats.core ->=]
            ;; (recur ctx (macroexpand/expand-> filename expr))
            ;; [cats.core ->>=]
            ;; (recur ctx (macroexpand/expand->> filename expr))
            ;; [rewrite-clj.custom-zipper.core defn-switchable]
            ;; (analyze-defn ctx expr)
            ;; ([clojure.core.async go-loop] [cljs.core.async go-loop] [cljs.core.async.macros go-loop])
            ;; (analyze-loop ctx expr)
            ;; catch-all
            (let [fn-name (when ?full-fn-name (symbol (name ?full-fn-name)))]
              (if (symbol? fn-name)
                (let [binding-call? (contains? bindings fn-name)]
                  (if binding-call?
                    (analyze-children next-ctx (rest children))
                    (let [call {:type :call
                                :name ?full-fn-name
                                :arity arg-count
                                :row row
                                :col col
                                :lang lang
                                :expr expr
                                :parents (:parents ctx)}
                          next-ctx (cond-> next-ctx
                                     (contains? '#{[clojure.core.async thread]}
                                                [resolved-namespace resolved-name])
                                     (assoc-in [:recur-arity :fixed-arity] 0))]
                      (cons call (analyze-children next-ctx (rest children))))))
                (analyze-children ctx children)))))))))

;; TODO:
;; Add test for lift-meta on anon fn
;; clojure/core/async.clj:508:31: error: recur argument count mismatch (expected 7, got 0)

;; rewrite_clj/custom_zipper/core.clj:134:9: error: unexpected recur
;; rewrite_clj/custom_zipper/core.clj:152:5: error: unexpected recur


(defn analyze-expression*
  [filename lang expanded-lang ns results expression debug?]
  (let [ctx {:filename filename
             :lang expanded-lang
             :ns ns
             :bindings #{}}]
    (loop [ns ns
           [first-parsed & rest-parsed :as all] (analyze-expression** ctx expression)
           results results]
      (if (seq all)
        (case (:type first-parsed)
          nil (recur ns rest-parsed results)
          (:ns :in-ns)
          (do
            ;; store namespace for future use
            (swap! namespace/namespaces assoc (:name first-parsed) first-parsed)
            (recur
             first-parsed
             rest-parsed
             (-> results
                 (assoc :ns first-parsed)
                 (update
                  :loaded into (:loaded first-parsed)))))
          (:duplicate-map-key
           :missing-map-value
           :duplicate-set-key
           :invalid-bindings
           :invalid-arity)
          (recur
           ns
           rest-parsed
           (update results
                   :findings conj (assoc first-parsed
                                         :filename filename)))
          ;; catch-all
          (recur
           ns
           rest-parsed
           (case (:type first-parsed)
             :debug
             (if debug?
               (update-in results
                          [:findings]
                          conj
                          (assoc first-parsed
                                 :filename filename))
               results)
             (let [resolved (resolve-name ns (:name first-parsed))
                   first-parsed (cond->
                                    (assoc first-parsed
                                           :name (:name resolved)
                                           :ns (:name ns))
                                  ;; if defined in CLJC file, we add that as the base-lang
                                  (= :cljc lang)
                                  (assoc :base-lang lang))]
               (case (:type first-parsed)
                 :defn
                 (let [path (case lang
                              :cljc [:defs (:name ns) (:lang first-parsed) (:name resolved)]
                              [:defs (:name ns) (:name resolved)])
                       results
                       (if resolved
                         (assoc-in results path
                                   (dissoc first-parsed
                                           :type))
                         results)]
                   (if debug?
                     (update-in results
                                [:findings]
                                vconj
                                (assoc first-parsed
                                       :level :info
                                       :filename filename
                                       :message
                                       (str/join " "
                                                 ["Defn resolved as"
                                                  (str (:ns resolved) "/" (:name resolved)) "with arities"
                                                  "fixed:"(:fixed-arities first-parsed)
                                                  "varargs:"(:var-args-min-arity first-parsed)])
                                       :type :debug))
                     results))
                 :call
                 (if resolved
                   (let [path [:calls (:ns resolved)]
                         unqualified? (:unqualified? resolved)
                         call (cond-> (assoc first-parsed
                                             :filename filename
                                             :resolved-ns (:ns resolved)
                                             :ns-lookup ns)
                                (:clojure-excluded? resolved)
                                (assoc :clojure-excluded? true)
                                unqualified?
                                (assoc :unqualified? true))
                         results (cond-> (update-in results path vconj call)
                                   (not unqualified?)
                                   (update :loaded conj (:ns resolved)))]
                     (if debug? (update-in results [:findings] conj
                                           (assoc call
                                                  :level :info
                                                  :message (str "Call resolved as "
                                                                (str (:ns resolved) "/" (:name resolved)))
                                                  :type :debug))
                         results))
                   (if debug?
                     (update-in results
                                [:findings]
                                conj
                                (assoc first-parsed
                                       :level :info
                                       :message (str "Unrecognized call to "
                                                     (:name first-parsed))
                                       :type :debug))
                     results))
                 results)))))
        [ns results]))))

(defn analyze-expressions
  "Analyzes expressions and collects defs and calls into a map. To
  optimize cache lookups later on, calls are indexed by the namespace
  they call to, not the ns where the call occurred. Also collects
  other findings and passes them under the :findings key."
  ([filename lang expressions] (analyze-expressions filename lang lang expressions))
  ([filename lang expanded-lang expressions] (analyze-expressions filename lang expanded-lang expressions false))
  ([filename lang expanded-lang expressions debug?]
   (profiler/profile
    :analyze-expressions
    (loop [ns (analyze-ns-decl expanded-lang (parse-string "(ns user)"))
           [expression & rest-expressions] expressions
           results {:calls {}
                    :defs {}
                    :loaded (:loaded ns)
                    :findings []
                    :lang lang}]
      (if expression
        (let [[ns results]
              (analyze-expression* filename lang expanded-lang ns results expression debug?)]
          (recur ns rest-expressions results))
        results)))))

;;;; processing of string input

(defn analyze-input
  "Analyzes input and returns analyzed defs, calls. Also invokes some
  linters and returns their findings."
  [filename input lang dev?]
  (try
    (let [parsed (p/parse-string input)
          nls (l/redundant-let filename parsed)
          ods (l/redundant-do filename parsed)
          findings {:findings (concat nls ods)
                    :lang lang}
          analyzed-expressions
          (case lang :cljc
                (let [clj (analyze-expressions filename lang
                                               :clj (:children (select-lang parsed :clj)))
                      cljs (analyze-expressions filename lang
                                                :cljs (:children (select-lang parsed :cljs)))]
                  (profiler/profile :deep-merge
                                    (deep-merge clj cljs)))
                (analyze-expressions filename lang lang
                                     (:children parsed)))]
      [findings analyzed-expressions])
    (catch Exception e
      (if dev? (throw e)
          [{:findings [{:level :error
                        :filename filename
                        :col 0
                        :row 0
                        :message (str "can't parse "
                                      filename ", "
                                      (.getMessage e))}]}]))
    (finally
      (when (-> @config/config :output :show-progress)
        (print ".") (flush)))))
