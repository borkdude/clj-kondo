(ns clj-kondo.unresolved-var-test
  (:require
    [clj-kondo.test-utils :refer [lint! assert-submaps]]
    [clojure.test :refer [deftest is testing]]))

(deftest unresolved-var-test
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 35, :level :error, :message "No such var: set/onion"})
   (lint! "(require '[clojure.set :as set]) (set/onion) set/union"
          '{:linters {:unresolved-var {:level :error}}}))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 41, :level :error, :message "No such var: set/onion"})
   (lint! "(require '[clojure.set :as set]) (apply set/onion 1 2 3)"
          '{:linters {:unresolved-var {:level :error}}}))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 48, :level :error, :message "No such var: foo/bar"})
   (lint! "(ns foo) (defn foo []) (ns bar (:require foo)) foo/bar"
          '{:linters {:unresolved-var {:level :error}}}))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 2, :level :error, :message "No such var: clojure.core/x"})
   (lint! "(clojure.core/x 1 2 3)"))
  (testing "vars from unknown namespaces are ignored"
    (is (empty?
         (lint! "(ns bar (:require foo)) foo/bar"
                '{:linters {:unresolved-var {:level :error}}}))))
  (is (empty?
       (lint! "(do 1 2) goog.global"
              '{:linters {:unresolved-var {:level :error}}}
              "--lang" "cljs"))))
