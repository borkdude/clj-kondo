(ns clj-kondo.unused-bindings-test
  (:require [clj-kondo.test-utils :refer [assert-submaps lint!]]
            [clojure.test :refer [deftest is testing]]
            [missing.test.assertions]))

(deftest unused-binding-test
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 7, :level :warning, :message "unused binding x"})
   (lint! "(let [x 1])" '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 12,
      :level :warning,
      :message "unused binding x"})
   (lint! "(defn foo [x])"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 15,
      :level :warning,
      :message "unused binding id"})
   (lint! "(let [{:keys [patient/id order/id]} {}] id)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 14,
      :level :warning,
      :message "unused binding a"}
     {:file "<stdin>",
      :row 1,
      :col 23,
      :level :warning,
      :message "unused default for binding a"})
   (lint! "(fn [{:keys [:a] :or {a 1}}])"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 8,
      :level :warning,
      :message "unused binding x"}
     {:file "<stdin>",
      :row 1,
      :col 12,
      :level :warning,
      :message "unused binding y"})
   (lint! "(loop [x 1 y 2])"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 10,
      :level :warning,
      :message "unused binding x"})
   (lint! "(if-let [x 1] 1 2)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 11,
      :level :warning,
      :message "unused binding x"})
   (lint! "(if-some [x 1] 1 2)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 12,
      :level :warning,
      :message "unused binding x"})
   (lint! "(when-let [x 1] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 13,
      :level :warning,
      :message "unused binding x"})
   (lint! "(when-some [x 1] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :level :warning,
      :message "unused binding x"})
   (lint! "(for [x []] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :level :warning,
      :message "unused binding x"})
   (lint! "(doseq [x []] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:level :warning,
      :message "unused binding x"}
     {:level :warning,
      :message "unused binding y"})
   (lint! "(with-open [x ? y ?] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:level :warning,
      :message "unused binding x"})
   (lint! "(with-local-vars [x 1] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 7,
      :level :warning,
      :message "unused binding x"}
     {:file "<stdin>",
      :row 1,
      :col 22,
      :level :warning,
      :message "unused binding y"}
     {:file "<stdin>",
      :row 1,
      :col 33,
      :level :error,
      :message "clojure.core/inc is called with 0 args but expects 1"}
     {:file "<stdin>",
      :row 1,
      :col 46,
      :level :error,
      :message "clojure.core/pos? is called with 0 args but expects 1"})
   (lint! "(for [x [] :let [x 1 y x] :when (inc) :while (pos?)] 1)"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 48,
      :level :warning,
      :message "unused binding a"}
     {:file "<stdin>",
      :row 1,
      :col 52,
      :level :warning,
      :message "unused binding b"})
   (lint! "(ns foo (:require [cats.core :as c])) (c/mlet [a 1 b 2])"
          '{:linters {:unused-binding {:level :warning}}
            :lint-as {cats.core/mlet clojure.core/let}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 24,
      :level :warning,
      :message "unused binding x"})
   (lint! "(defmacro foo [] (let [x 1] `(inc x)))"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 12,
      :level :warning,
      :message "unused binding x"})
   (lint! "(defn foo [x] (quote x))"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 17,
      :level :warning,
      :message "unused binding variadic"})
   (lint! "(let [{^boolean variadic :variadic?} {}] [])"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 8,
      :level :warning,
      :message "unused binding a"})
   (lint! "#(let [a %])"
          '{:linters {:unused-binding {:level :warning}}}))
  (assert-submaps
   '({:file "<stdin>",
      :row 1,
      :col 7,
      :level :warning,
      :message "unused binding a"})
   (lint! "(let [a 1] `{:a 'a})"
          '{:linters {:unused-binding {:level :warning}}}))
  (is (empty? (lint! "(let [{:keys [:a :b :c]} 1 x 2] (a) b c x)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defn foo [x] x)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defn foo [_x])"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(fn [{:keys [x] :or {x 1}}] x)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "#(inc %1)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(let [exprs []] (loop [exprs exprs] exprs))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(for [f fns :let [children (:children f)]] children)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(deftype Foo [] (doseq [[key f] []] (f key)))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defmacro foo [] (let [x 1] `(inc ~x)))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(let [[_ _ name] nil]
                        `(cljs.core/let [~name ~e] ~@cb))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defmacro foo [] (let [x 1] `(inc ~@[x])))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defn false-positive-metadata [a b] ^{:key (str a b)} [:other])"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(doseq [{ts :tests {:keys [then]} :then} nodes]
                        (doseq [test (map :test ts)] test)
                        then)"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(let [a 1] (cond-> (.getFoo a) x))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defmacro foo [] (let [sym 'my-symbol] `(do '~sym)))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(let [s 'clojure.string] (require s))"
                     '{:linters {:unused-binding {:level :warning}}})))
  (is (empty? (lint! "(defn f [{:keys [:a :b :c]}] a)"
                     '{:linters {:unused-binding
                                 {:level :warning
                                  :exclude-destructured-keys-in-fn-args true}
                                 :unresolved-symbol {:level :error}}})))
  (is (empty? (lint! "(ns problem {:clj-kondo/config {:linters {:unused-binding {:level :off}}}})
(defn f [x] (println))"
                     '{:linters {:unused-binding {:level :warning}}}))))

(deftest unused-destructuring-default-test
  (doseq [input ["(let [{:keys [:i] :or {i 2}} {}])"
                 "(let [{:or {i 2} :keys [:i]} {}])"
                 "(let [{:keys [:i :j] :or {i 2 j 3}} {}] j)"]]
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :level :warning
                       :message "unused binding i"}
                      {:file "<stdin>"
                       :row 1
                       :level :warning
                       :message "unused default for binding i"})
                    (lint! input
                           '{:linters
                             {:unused-binding {:level :warning}}})))
  (testing "finding points at the symbol of the default"
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :col 15
                       :level :warning
                       :message "unused binding i"}
                      {:file "<stdin>"
                       :row 1
                       :col 24
                       :level :warning
                       :message "unused default for binding i"})
                    (lint! "(let [{:keys [:i] :or {i 2}} {}] nil)"
                           '{:linters
                             {:unused-binding {:level :warning}}})))
  (testing "respects the :exclude-destructured-keys-in-fn-args setting from the "
    (is (empty? (lint! "(defn f [{:keys [:a] :or {a 1}}] nil)"
                       '{:linters {:unused-binding
                                   {:level :warning
                                    :exclude-destructured-keys-in-fn-args true}}}))))
  (testing "respects the :exclude-destructured-as as true setting from the "
    (is (empty? (lint! "(defn f [{:keys [:a] :as config}] a)"
                       '{:linters {:unused-binding
                                   {:level :warning
                                    :exclude-destructured-as true}}}))))
  (testing "respects the :exclude-destructured-as as true and also shows unused other bindings setting from the "
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :col 18
                       :level :warning
                       :message "unused binding a"})
                    (lint! "(defn f [{:keys [:a] :as config}] nil)"
                       '{:linters {:unused-binding
                                   {:level :warning
                                    :exclude-destructured-as true}}}))
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :col 18
                       :level :warning
                       :message "unused binding a"})
                    (lint! "(defn f [{:keys [:a] :as config}] config)"
                       '{:linters {:unused-binding
                                   {:level :warning
                                    :exclude-destructured-as true}}})))
  (testing "respects the :exclude-destructured-as as false setting from the "
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :col 26
                       :level :warning
                       :message "unused binding config"})
                    (lint! "(defn f [{:keys [:a] :as config}] a)"
                           '{:linters {:unused-binding
                                       {:level :warning
                                        :exclude-destructured-as false}}})))
  (testing "respects the :exclude-destructured-as as false setting and also shows all unused bindings from the "
    (assert-submaps '({:file "<stdin>"
                       :row 1
                       :col 18
                       :level :warning
                       :message "unused binding a"}
                      {:file "<stdin>"
                       :row 1
                       :col 26
                       :level :warning
                       :message "unused binding config"})
                    (lint! "(defn f [{:keys [:a] :as config}] nil)"
                           '{:linters {:unused-binding
                                       {:level :warning
                                        :exclude-destructured-as false}}}))))
