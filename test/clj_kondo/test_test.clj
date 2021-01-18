(ns clj-kondo.test-test
  (:require
    [clj-kondo.test-utils :refer [lint! assert-submaps]]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is]]))

(deftest missing-test-assertion-test
  (is (empty? (lint! "(ns foo (:require [clojure.test :as t])) (t/deftest (t/is (odd? 1)))")))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 57, :level :warning, :message "missing test assertion"})
   (lint! "(ns foo (:require [clojure.test :as t])) (t/deftest foo (odd? 1))"))
  (assert-submaps
   '({:file "<stdin>", :row 2, :col 21, :level :warning, :message "missing test assertion"})
   (lint! "(ns foo (:require [clojure.test :as t] [clojure.set :as set]))
     (t/deftest foo (set/subset? #{1 2} #{1 2 3}))"))
  (assert-submaps
   '({:file "<stdin>", :row 2, :col 38, :level :warning, :message "missing test assertion"})
   (lint! "(ns foo (:require [clojure.test :as t] [clojure.set :as set]))
     (t/deftest foo (t/testing \"foo\" (set/subset? #{1 2} #{1 2 3})))"))
  (assert-submaps
   '({:file "<stdin>", :row 2, :col 49, :level :warning, :message "missing test assertion"})
   (lint! "(ns foo (:require [clojure.test :as t] [clojure.set :as set]))
     (t/deftest foo (t/testing \"foo\" (let [x 1] (set/subset? #{1 2} #{1 2 3}))))")))

(deftest redefined-test-test
  (assert-submaps
   '({:file "corpus/redefined_deftest.clj",
      :row 4,
      :col 1,
      :level :error,
      :message "clojure.test/deftest is called with 0 args but expects 1 or more"}
     {:file "corpus/redefined_deftest.clj",
      :row 7,
      :col 1,
      :level :warning,
      :message "redefined var #'redefined-deftest/foo"}
     {:file "corpus/redefined_deftest.clj",
      :row 9,
      :col 1,
      :level :error,
      :message "redefined-deftest/foo is called with 1 arg but expects 0"})
   (lint! (io/file "corpus" "redefined_deftest.clj")))
  (is (empty?
       (lint! "(ns foo (:require [clojure.test :refer :all])) (deftest foo (is (empty? #{})))"
              {:linters {:unresolved-symbol {:level :info}}}))))

(deftest cljs-async-test
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 46,
      :level :error,
      :message "foo is called with 1 arg but expects 0"})
   (lint! "(require '[clojure.test :as t]) (t/async foo (foo 1))"
          "--lang" "cljs")))

(deftest deftest-test
  (is (empty? (lint! (io/file "corpus" "deftest.cljc")
                     '{:linters {:unresolved-symbol {:level :error}}}))))

(deftest are-test
  (assert-submaps
   [{:file "corpus/clojure.test.are.cljc", :row 9, :col 11,
     :level :warning, :message "unused binding a"}
    {:file "corpus/clojure.test.are.cljc", :row 9, :col 21,
     :level :error, :message "Unresolved symbol: z"}]
   (lint! (io/file "corpus" "clojure.test.are.cljc")
          {:linters {:unresolved-symbol {:level :error}
                     :unused-binding {:level :warning}}})))
