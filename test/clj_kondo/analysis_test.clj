(ns clj-kondo.analysis-test
  (:require
   [clj-kondo.core :as clj-kondo]
   [clj-kondo.test-utils :refer [assert-submaps]]
   [clojure.test :as t :refer [deftest is testing]]))

(defn analyze
  ([input] (analyze input nil))
  ([input config]
   (:analysis
    (with-in-str
      input
      (clj-kondo/run! (merge
                       {:lint ["-"]
                        :config {:output {:analysis true}}}
                       config))))))

(deftest analysis-test
  (let [{:keys [:var-definitions
                :var-usages]} (analyze "(defn ^:deprecated foo \"docstring\" {:added \"1.2\"} [])")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :ns user,
        :name foo,
        :fixed-arities #{0},
        :doc "docstring",
        :added "1.2",
        :deprecated true}]
     var-definitions)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :from user,
        :to clojure.core,
        :name defn,
        :arity 4}]
     var-usages))

  (let [{:keys [:var-definitions]} (analyze "(def ^:deprecated x \"docstring\" 1)")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :ns user,
        :name x,
        :doc "docstring",
        :deprecated true}]
     var-definitions))
  (let [{:keys [:namespace-definitions
                :namespace-usages]}
        (analyze
         "(ns ^:deprecated foo \"docstring\"
            {:added \"1.2\" :no-doc true :author \"Michiel Borkent\"}
            (:require [clojure.string]))")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :name foo,
        :deprecated true,
        :doc "docstring",
        :added "1.2",
        :no-doc true,
        :author "Michiel Borkent"}]
     namespace-definitions)
    (assert-submaps
     '[{:filename "<stdin>", :row 3, :col 24, :from foo, :to clojure.string}]
     namespace-usages))
  (let [{:keys [:namespace-definitions
                :namespace-usages
                :var-usages
                :var-definitions]}
        (analyze "(ns foo (:require [clojure.string]))
                  (defn f [] (inc 1 2 3))" {:lang :cljc})]
    (assert-submaps
     '[{:filename "<stdin>", :row 1, :col 1, :name foo, :lang :clj}
       {:filename "<stdin>", :row 1, :col 1, :name foo, :lang :cljs}]
     namespace-definitions)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 20,
        :from foo,
        :to clojure.string,
        :lang :clj}
       {:filename "<stdin>",
        :row 1,
        :col 20,
        :from foo,
        :to clojure.string,
        :lang :cljs}]
     namespace-usages)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 2,
        :col 19,
        :ns foo,
        :name f,
        :fixed-arities #{0},
        :lang :clj}
       {:filename "<stdin>",
        :row 2,
        :col 19,
        :ns foo,
        :name f,
        :fixed-arities #{0},
        :lang :cljs}]
     var-definitions)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 2,
        :col 30,
        :from foo,
        :to clojure.core,
        :name inc,
        :fixed-arities #{1},
        :arity 3,
        :lang :clj}
       {:name defn,
        :var-args-min-arity 2,
        :lang :clj,
        :filename "<stdin>",
        :from foo,
        :macro true,
        :col 19,
        :arity 3,
        :row 2,
        :to clojure.core}
       {:filename "<stdin>",
        :row 2,
        :col 30,
        :from foo,
        :to cljs.core,
        :name inc,
        :fixed-arities #{1},
        :arity 3,
        :lang :cljs}
       {:name defn,
        :var-args-min-arity 2,
        :lang :cljs,
        :filename "<stdin>",
        :from foo,
        :macro true,
        :col 19,
        :arity 3,
        :row 2,
        :to cljs.core}]
     var-usages)))
