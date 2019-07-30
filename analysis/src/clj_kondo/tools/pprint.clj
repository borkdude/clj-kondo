(ns clj-kondo.tools.pprint
  (:require
   [clj-kondo.core :as clj-kondo]
   [clojure.pprint :as p]))

(defn- private-fixed-arity [_x _y _z])

(defmacro macro-var-args-arity [_x & _xs])

(defn -main [format & paths]
  (let [analysis (:analysis (clj-kondo/run! {:lint paths :config {:output {:analysis true}}}))
        {:keys [:namespace-definitions
                :namespace-usages
                :var-definitions
                :var-usages]} analysis]
    (case format
      "table"
      (do
        (print ":namespace-definitions")
        (p/print-table [:filename :row :col :name :doc :author :added :deprecated :no-doc]
                       namespace-definitions)
        (println)
        (print ":namespace-usages")
        (p/print-table [:filename :row :col :from :to]
                       namespace-usages)
        (println)
        (print ":var-definitions")
        (p/print-table [:filename :row :col :ns :name :doc :fixed-arities :var-args-min-arity :private :macro :added :deprecated]
                       var-definitions)
        (println)
        (print ":var-usages")
        (p/print-table var-usages))
      "edn"
      (p/pprint analysis)
      (println "use table or edn as first argument"))))
