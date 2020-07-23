(ns clj-kondo.format-test
  (:require
   [clj-kondo.test-utils :refer
    [lint! assert-submaps]]
   [clojure.test :as t :refer [deftest is testing]]))

(deftest format-test
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 1, :level :error, :message "Format string expects 2 arguments instead of 1."})
   (lint! "(format \"%s %s\" 1)"))
  (is (empty? (lint! "(defn foo [x] (format x 1))"))))
