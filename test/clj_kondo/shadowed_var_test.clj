(ns clj-kondo.shadowed-var-test
  (:require
   [clj-kondo.test-utils :refer [lint! assert-submaps]]
   [clojure.test :refer [deftest is testing]]))

(deftest shadowed-var-test
  (assert-submaps
   '({:file "<stdin>", :row 4, :col 15, :level :warning,
      :message "Shadowed var: clojure.core/name. Suggestion: nom"}
     {:file "<stdin>", :row 7, :col 7, :level :warning,
      :message "Shadowed var: clojure.core/name. Suggestion: nom"})
   (lint! "
(ns foo)

(defn foo [ns name] ;; only warning about name
  [ns name])

(let [name 1] ;; also works in let
  name)

(defn bar [ns #_:clj-kondo/ignore name] ;; ignored
  [ns name])
"
          '{:linters {:shadowed-var {:level :warning
                                     :exclude [ns]
                                     :suggest {name nom}}}}))
  (assert-submaps
   '({:file "<stdin>", :row 8, :col 7, :level :warning,
      :message "Shadowed var: clojure.core/name. Suggestion: dude"})
   (lint! "
(ns foo
  {:clj-kondo/config '{:linters {:shadowed-var
                                 {:level :warning
                                  :include [name]
                                  :suggest {name dude}}}}})

(let [name 1
      frequencies 2]
  (+ name frequencies))"))
  (is (empty? (lint! "(ns foo (:refer-clojure :exclude [ns-name])) (defn foo [ns-name] ns-name)"
                     '{:linters {:shadowed-var {:level :warning
                                                :exclude [ns]
                                                :suggest {name nom}}}})))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 17, :level :warning, :message "Shadowed var: user/x"})
   (lint! "(def x 1) (let [x 2] x)"
          '{:linters {:shadowed-var {:level :warning}}})))
