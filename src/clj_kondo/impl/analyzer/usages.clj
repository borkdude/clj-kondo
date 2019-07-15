(ns clj-kondo.impl.analyzer.usages
  {:no-doc true}
  (:require
   [clj-kondo.impl.namespace :as namespace :refer [resolve-name]]
   [clj-kondo.impl.utils :as utils :refer
    [symbol-call keyword-call node->line
     parse-string parse-string-all tag select-lang
     vconj deep-merge one-of symbol-from-token tag
     kw->sym]]))

(set! *warn-on-reflection* true)

(defn analyze-keyword [ctx expr]
  (let [ns (:ns ctx)
        ns-name (:name ns)
        keyword-val (:k expr)]
    (when (:namespaced? expr)
      (let [symbol-val (kw->sym keyword-val)
            {resolved-ns :ns
             _resolved-name :name
             _unqualified? :unqualified? :as _m}
            (namespace/resolve-name ctx ns-name symbol-val)]
        (when resolved-ns
          (namespace/reg-usage! ctx
                                (-> ns :name)
                                resolved-ns))))))

(defn analyze-usages2
  ([ctx expr] (analyze-usages2 ctx expr {}))
  ([ctx expr {:keys [:quote? :syntax-quote?] :as opts}]
   (let [ns (:ns ctx)
         ns-name (:name ns)
         tag (tag expr)
         quote? (or quote? (= :quote tag))]
     (if (one-of tag [:unquote :unquote-splicing])
       (when-let [f (:analyze-expression** ctx)]
         (f ctx expr))
       (when (or (not quote?)
                 ;; when we're in syntax-quote, we should still look for
                 ;; unquotes, since these will be evaluated first
                 syntax-quote?)
         (let [syntax-quote?
               (or syntax-quote?
                   (= :syntax-quote tag))]
           (case tag
             :token
             (if-let [symbol-val (symbol-from-token expr)]
               (let [simple-symbol? (empty? (namespace symbol-val))]
                 (if-let [b (when (and simple-symbol? (not syntax-quote?))
                              (get (:bindings ctx) symbol-val))]
                   (namespace/reg-used-binding! ctx
                                                (-> ns :name)
                                                b)
                   (if-let [resolved-ns (when simple-symbol?
                                          (get (:qualify-ns ns) symbol-val))]
                     (namespace/reg-usage! ctx
                                           (-> ns :name)
                                           resolved-ns)
                     (let [{resolved-ns :ns
                            resolved-name :name
                            unqualified? :unqualified? :as _m} (namespace/resolve-name ctx ns-name symbol-val)
                           m (meta expr)
                           {:keys [:row :col]} m]
                       (when (and unqualified? (not syntax-quote?))
                         (namespace/reg-unresolved-symbol! ctx ns-name symbol-val m))
                       (when resolved-ns
                         (namespace/reg-usage! ctx
                                               ns-name
                                               resolved-ns)
                         (namespace/reg-var-usage! ctx ns-name
                                                   {:type :use
                                                    :name resolved-name
                                                    :resolved-ns resolved-ns
                                                    :ns ns-name
                                                    :unqualified? unqualified?
                                                    :row row
                                                    :col col
                                                    :base-lang (:base-lang ctx)
                                                    :lang (:lang ctx)
                                                    :filename (:filename ctx)
                                                    :private-access? (:private-access? ctx)}))))))
               (when (:k expr)
                 (analyze-keyword ctx expr)))
             ;; catch-call
             (doall (mapcat
                     #(analyze-usages2 ctx % (assoc opts :quote? quote? :syntax-quote? syntax-quote?))
                     (:children expr))))))))))
