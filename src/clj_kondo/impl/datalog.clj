(ns clj-kondo.impl.datalog
  {:no-doc true}
  (:require
   [clj-kondo.impl.findings :as findings]
   [clj-kondo.impl.utils :as utils :refer
    [node->line tag one-of tag sexpr]]
   [datalog.parser :as datalog]))

(defn analyze-datalog [{:keys [:findings] :as ctx} expr]
  (let [children (next (:children expr))
        query-raw (first children)
        quoted? (when query-raw
                  (= :quote (tag query-raw)))
        datalog-node (when quoted?
                       (when-let [edn-node (first (:children query-raw))]
                         (when (one-of (tag edn-node) [:vector :map])
                           edn-node)))]
    (when datalog-node
      (try
        (datalog/parse (sexpr datalog-node))
        nil
        (catch Exception e
          (findings/reg-finding! findings
                                 (node->line (:filename ctx) query-raw
                                             :error :datalog-syntax
                                             (.getMessage e))))))
    ;; lint all children regardless if it was datalog to get additional EDN feedback
    ))
