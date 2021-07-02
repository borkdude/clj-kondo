(ns clj-kondo.analysis-test
  (:require
   [clj-kondo.core :as clj-kondo]
   [clj-kondo.impl.utils :refer [err]]
   [clj-kondo.test-utils :refer [assert-submaps]]
   [clojure.edn :as edn]
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

(deftest keyword-analysis-test
  (testing "standalone keywords with top-level require"
    (let [a (analyze "(require '[bar :as b]) :kw :x/xkwa ::x/xkwb ::fookwa :foo/fookwb ::foo/fookwc :bar/barkwa ::b/barkwb ::bar/barkwc"
                     {:config {:output {:analysis {:keywords true}}}})]
      (assert-submaps
        '[{:name "kw"}
          {:name "xkwa" :ns x}
          {:name "xkwb" :ns :clj-kondo/unknown-namespace}
          {:name "fookwa" :ns user}
          {:name "fookwb" :ns foo}
          {:name "fookwc" :ns :clj-kondo/unknown-namespace}
          {:name "barkwa" :ns bar}
          {:name "barkwb" :ns bar :alias b}
          {:name "barkwc" :ns :clj-kondo/unknown-namespace}]
        (:keywords a))))
  (testing "standalone keywords"
    (let [a (analyze "(ns foo (:require [bar :as b])) :kw :2foo :x/xkwa ::x/xkwb ::fookwa :foo/fookwb ::foo/fookwc :bar/barkwa ::b/barkwb ::bar/barkwc"
                     {:config {:output {:analysis {:keywords true}}}})]
      (assert-submaps
        '[{:name "kw"}
          {:name "2foo"}
          {:name "xkwa" :ns x}
          {:name "xkwb" :ns :clj-kondo/unknown-namespace}
          {:name "fookwa" :ns foo}
          {:name "fookwb" :ns foo}
          {:name "fookwc" :ns :clj-kondo/unknown-namespace}
          {:name "barkwa" :ns bar}
          {:name "barkwb" :ns bar :alias b}
          {:name "barkwc" :ns :clj-kondo/unknown-namespace}]
        (:keywords a))))
  (testing "destructuring keywords"
    (let [a (analyze (str "(ns foo (:require [bar :as b]))\n"
                          "(let [{::keys [a :b]\n"
                          "       ::b/keys [c :d]\n"
                          "       :bar/keys [e :f]\n"
                          "       :keys [g :h ::i :foo/j :bar/k ::b/l ::bar/m :x/n ::y/o]\n"
                          "       p :p q ::q r ::b/r s :bar/s t :x/t} {}])")
                     {:config {:output {:analysis {:keywords true}}}})]
      (assert-submaps
        '[{:name "keys" :ns foo}
          {:name "a" :ns foo :keys-destructuring true}
          {:name "b" :ns foo :keys-destructuring true}
          {:name "keys" :ns bar :alias b}
          {:name "c" :ns bar :keys-destructuring true}
          {:name "d" :ns bar :keys-destructuring true}
          {:name "keys" :ns bar}
          {:name "e" :ns bar :keys-destructuring true}
          {:name "f" :ns bar :keys-destructuring true}
          {:name "keys"}
          {:name "g" :keys-destructuring true}
          {:name "h" :keys-destructuring true}
          {:name "i" :ns foo :keys-destructuring true}
          {:name "j" :ns foo :keys-destructuring true}
          {:name "k" :ns bar :keys-destructuring true}
          {:name "l" :ns bar :alias b :keys-destructuring true}
          {:name "m" :ns :clj-kondo/unknown-namespace}
          {:name "n" :ns x}
          {:name "o" :ns :clj-kondo/unknown-namespace}
          {:name "p"}
          {:name "q" :ns foo}
          {:name "r" :ns bar :alias b}
          {:name "s" :ns bar}
          {:name "t" :ns x}]
        (:keywords a))))
  (testing "clojure.spec.alpha/def can add :reg"
    (let [a (analyze "(require '[clojure.spec.alpha :as s]) (s/def ::kw (inc))"
                     {:config {:output {:analysis {:keywords true}}}})]
      (assert-submaps
        '[{:name "kw" :reg clojure.spec.alpha/def}]
        (:keywords a))))
  (testing "re-frame.core/reg-event-db can add :reg"
    (let [a (analyze "(require '[re-frame.core :as rf])
                      (rf/reg-event-db ::a (constantly {}))
                      (rf/reg-event-fx ::b (constantly {}))
                      (rf/reg-event-ctx ::c (constantly {}))
                      (rf/reg-sub ::d (constantly {}))
                      (rf/reg-sub-raw ::e (constantly {}))
                      (rf/reg-fx ::f (constantly {}))
                      (rf/reg-cofx ::g (constantly {}))"
                     {:config {:output {:analysis {:keywords true}}}})]
      (assert-submaps
        '[{:name "a" :reg re-frame.core/reg-event-db}
          {:name "b" :reg re-frame.core/reg-event-fx}
          {:name "c" :reg re-frame.core/reg-event-ctx}
          {:name "d" :reg re-frame.core/reg-sub}
          {:name "e" :reg re-frame.core/reg-sub-raw}
          {:name "f" :reg re-frame.core/reg-fx}
          {:name "g" :reg re-frame.core/reg-cofx}]
        (:keywords a))))
  (testing ":lint-as re-frame.core function will add :reg with the source full qualified ns"
    (let [a (analyze "(user/mydef ::kw (constantly {}))"
                     {:config {:output {:analysis {:keywords true}}
                               :lint-as '{user/mydef re-frame.core/reg-event-fx}}})]
      (assert-submaps
        '[{:name "kw" :reg user/mydef}]
        (:keywords a))))
  (testing "hooks can add :reg"
    (let [a (analyze "(user/mydef ::kw (inc))"
                     {:config {:output {:analysis {:keywords true}}
                               :hooks {:__dangerously-allow-string-hooks__ true
                                       :analyze-call
                                       {'user/mydef
                                        (str "(require '[clj-kondo.hooks-api :as a])"
                                             "(fn [{n :node}]"
                                             "  (let [c (:children n)]"
                                             "    {:node (a/list-node "
                                             "             (list* 'do"
                                             "                    (a/reg-keyword! (second c) 'user/mydef)"
                                             "                    (drop 2 c)))}))")}}}})]
      (assert-submaps
        '[{:name "kw" :reg user/mydef}]
        (:keywords a))))
  (testing "valid ns name with clojure.data.xml"
    (let [a (analyze "(ns foo (:require [clojure.data.xml :as xml]))
                      (xml/alias-uri 'pom \"http://maven.apache.org/POM/4.0.0\")
                      ::pom/foo"
                     {:config {:output {:analysis {:keywords true}}}})]
      (is (edn/read-string (str a)))))
  (testing "namespaced maps"
    (testing "auto-resolved namespace"
      (let [a (analyze "(ns foo (:require [clojure.data.xml :as xml]))
                      #::xml{:a 1}"
                       {:config {:output {:analysis {:keywords true}}}})]
        (assert-submaps
         '[{:name "a" :ns clojure.data.xml}]
         (:keywords a))))
    (testing "non-autoresolved namespace"
      (let [a (analyze "(ns foo (:require [clojure.data.xml :as xml]))
                      #:xml{:a 1}"
                       {:config {:output {:analysis {:keywords true}}}})]
        (assert-submaps
         '[{:name "a" :ns xml}]
         (:keywords a))))
    ;; Don't use assertmap here to make sure ns is absent
    (testing "no namespace for key :a"
      (let [a (analyze "#:xml{:_/a 1}"
                       {:config {:output {:analysis {:keywords true}}}})]
        (is (= '[{:row 1, :col 7, :end-row 1, :end-col 11, :name "a", :filename "<stdin>"}]
               (:keywords a)))))
    ;; Don't use assertmap here to make sure ns is absent
    (testing "no namespace for key :b"
      (let [a (analyze "#:xml{:a {:b 1}}"
                       {:config {:output {:analysis {:keywords true}}}})]
        (is (= '[{:row 1, :col 7, :end-row 1, :end-col 9, :ns xml, :name "a", :filename "<stdin>" :namespace-from-prefix true}
                 {:row 1, :col 11, :end-row 1, :end-col 13, :name "b", :filename "<stdin>"}]
          (:keywords a)))))
    (testing "auto-resolved and namespace-from-prefix"
      (let [a (analyze "(ns foo (:require [clojure.set :as set]))
                        :a ::b :bar/c
                        #:d{:e 1 :_/f 2 :g/h 3 ::i 4}
                        {:j/k 5 :l 6 ::m 7}
                        #::set{:a 1}"
                       {:config {:output {:analysis {:keywords true}}}})]
        (is (= '[{:row 2 :col 25 :end-row 2 :end-col 27 :name "a" :filename "<stdin>"}
                 {:row 2 :col 28 :end-row 2 :end-col 31 :ns foo :auto-resolved true :name "b" :filename "<stdin>"}
                 {:row 2 :col 32 :end-row 2 :end-col 38 :ns bar :name "c" :filename "<stdin>"}
                 {:row 3 :col 29 :end-row 3 :end-col 31 :ns d :namespace-from-prefix true :name "e" :filename "<stdin>"}
                 {:row 3 :col 34 :end-row 3 :end-col 38 :name "f" :filename "<stdin>"}
                 {:row 3 :col 41 :end-row 3 :end-col 45 :ns g :name "h" :filename "<stdin>"}
                 {:row 3 :col 48 :end-row 3 :end-col 51 :ns foo :auto-resolved true :name "i" :filename "<stdin>"}
                 {:row 4 :col 26 :end-row 4 :end-col 30 :ns j :name "k" :filename "<stdin>"}
                 {:row 4 :col 33 :end-row 4 :end-col 35 :name "l" :filename "<stdin>"}
                 {:row 4 :col 38 :end-row 4 :end-col 41 :ns foo :auto-resolved true :name "m" :filename "<stdin>"}
                 {:row 5, :col 32, :end-row 5, :end-col 34, :ns clojure.set, :namespace-from-prefix true,
                  :name "a", :filename "<stdin>"}]
               (:keywords a)))))))

(deftest locals-analysis-test
  (let [a (analyze "#(inc %1 %&)" {:config {:output {:analysis {:locals true}}}})]
    (is (= [] (:locals a) (:local-usages a))))
  (let [a (analyze "(areduce [] i j 0 (+ i j))" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 14 :scope-end-col 27}
       {:end-col 16 :scope-end-col 27}]
      (:locals a))
    (is (= (:id first-a) (:id first-use)))
    (is (= (:id second-a) (:id second-use))))
  (let [a (analyze "(defn x [a] (let [a a] a) a)" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use third-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 11 :scope-end-col 29}
       {:end-col 20 :scope-end-col 26}]
      (:locals a))
    (is (= (:id first-a) (:id first-use) (:id third-use)))
    (is (= (:id second-a) (:id second-use))))
  (let [a (analyze "(as-> {} $ $)" {:config {:output {:analysis {:locals true}}}})
        [first-a] (:locals a)
        [first-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 11 :scope-end-col 14}]
      (:locals a))
    (is (= (:id first-a) (:id first-use))))
  (let [a (analyze "(letfn [(a [b] b)] a)" {:config {:output {:analysis {:locals true}}}})
         [first-a second-a] (:locals a)
         [first-use second-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 11 :scope-end-col 22}
       {:end-col 14 :scope-end-col 18}]
      (:locals a))
    (is (= (:id first-a) (:id first-use)))
    (is (= (:id second-a) (:id second-use))))
  (let [a (analyze "(let [a 0] (let [a a] a))" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 8 :scope-end-col 26}
       {:end-col 19 :scope-end-col 25}]
      (:locals a))
    (is (= (:id first-a) (:id first-use)))
    (is (= (:id second-a) (:id second-use))))
  (let [a (analyze "(let [a 0 a a] a)" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 8 :scope-end-col 18}
       {:end-col 12 :scope-end-col 18}]
      (:locals a))
    (is (= (:id first-a) (:id first-use)))
    (is (= (:id second-a) (:id second-use))))
  (let [a (analyze "(if-let [a 0] a a)" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 11 :scope-end-col 16}]
      (:locals a))
    (is (= (:id first-a) (:id first-use)))
    (is (= nil second-a second-use)))
  (let [a (analyze "(for [a [123] :let [a a] :when a] a)" {:config {:output {:analysis {:locals true}}}})
        [first-a second-a] (:locals a)
        [first-use second-use third-use] (:local-usages a)]
    (assert-submaps
      [{:end-col 8 :scope-end-col 37}
       {:end-col 22 :scope-end-col 37}]
      (:locals a))
    (is (not= (:id first-a) (:id second-a)))
    (is (= (:id first-a) (:id first-use)))
    (is (= (:id second-a) (:id second-use) (:id third-use))))
  (testing "local usages are reported with correct positions"
    (let [ana (analyze "(let [x (set 1 2 3)] (+ x 1))" {:config {:output {:analysis {:locals true}}}})
          [x] (:locals ana)]
      (assert-submaps
        [{:row 1, :col 25,
          :end-row 1, :end-col 26,
          :name-row 1, :name-col 25,
          :name-end-row 1, :name-end-col 26,
          :name (:name x),
          :filename "<stdin>",
          :id 1}]
        (:local-usages ana))))
  (testing "Names are reported in binding usages when called as fn"
    (let [ana (analyze "(let [x #(set 1 2 3)] (x 1))" {:config {:output {:analysis {:locals true}}}})
          [x] (:locals ana)]
      (assert-submaps
        [{:row 1, :col 23,
          :end-col 28, :end-row 1,
          :name-row 1, :name-col 24,
          :name-end-col 25, :name-end-row 1,
          :name (:name x),
          :filename "<stdin>",
          :id 1}]
        (:local-usages ana))))
  (testing "generated nodes should not be included on analysis"
    (let [ana (analyze "(cond-> {:a 1 :b 2} true (merge {}))" {:config {:output {:analysis {:locals true}}}})]
      (is (empty? (:locals ana)))
      (is (empty? (:local-usages ana))))
    (let [ana (analyze "(doto {:a 1 :b 2} (merge {}))" {:config {:output {:analysis {:locals true}}}})]
      (is (empty? (:locals ana)))
      (is (empty? (:local-usages ana))))))

(deftest name-position-test
  (let [{:keys [:var-definitions :var-usages]} (analyze "(defn foo [] foo)" {:config {:output {:analysis {:locals true}}}})]
    (assert-submaps
     '[{:name foo :name-row 1 :name-col 7 :name-end-row 1 :name-end-col 10 :end-row 1 :end-col 18}]
     var-definitions)
    (assert-submaps
      '[{:name foo :name-row 1 :name-col 14 :name-end-row 1 :name-end-col 17} {}]
      var-usages))
  (let [{:keys [:var-definitions :var-usages]} (analyze "(defprotocol Foo (bar [])) Foo bar" {:config {:output {:analysis {:locals true}}}})]
    (assert-submaps
      '[{:name Foo :name-row 1 :name-col 14 :name-end-row 1 :name-end-col 17 :end-row 1 :end-col 27}
        {:name bar :name-row 1 :name-col 19 :name-end-row 1 :name-end-col 22 :end-row 1 :end-col 27}]
      var-definitions)
    (assert-submaps
      '[{}
        {:name Foo
         :name-end-col 31,
         :name-end-row 1,
         :name-row 1,
         :name-col 28}
        {:name bar
         :name-end-col 35,
         :name-end-row 1,
         :name-row 1,
         :name-col 32}]
      var-usages))
  (let [{:keys [:namespace-definitions :namespace-usages]} (analyze "(ns foo (:require [bar :as b :refer [x]] [clojure [string :as str]]))" {:config {:output {:analysis {:locals true}}}})]
    (assert-submaps
      '[{:name foo
         :name-end-col 8,
         :name-end-row 1,
         :name-row 1,
         :name-col 5}]
      namespace-definitions)
    (assert-submaps
      '[{:alias b
         :alias-end-col 29,
         :alias-end-row 1,
         :alias-row 1,
         :alias-col 28
         :name-row 1
         :name-col 20
         :name-end-row 1
         :name-end-col 23}
        {:alias str
         :alias-end-col 66,
         :alias-end-row 1,
         :alias-row 1,
         :alias-col 63
         :name-row 1
         :name-col 52
         :name-end-row 1
         :name-end-col 58}]
      namespace-usages))
  (let [{:keys [:var-definitions :var-usages]} (analyze "(try (catch Exception foo foo))" {:config {:output {:analysis {:locals true}}}})]
    (assert-submaps
      '[]
      var-definitions)
    (assert-submaps
      '[{:name-row 1 :name-col 13 :name-end-row 1 :name-end-col 22} {}]
      var-usages))
  (let [{:keys [:var-definitions :var-usages]} (analyze "(def a (atom nil)) (:foo @a)" {:config {:output {:analysis {:locals true}}}})]
    (assert-submaps
      '[{:name-row 1 :name-col 6 :name-end-row 1 :name-end-col 7 :end-row 1 :end-col 19}]
      var-definitions)
    (assert-submaps
      '[{} {} {:name-row 1 :name-col 27 :name-end-row 1 :name-end-col 28}]
      var-usages)))

(deftest analysis-test
  (let [{:keys [:var-definitions
                :var-usages]} (analyze "(defn ^:deprecated foo \"docstring\" {:added \"1.2\"} [])")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :end-row 1,
        :end-col 54,
        :ns user,
        :name foo,
        :defined-by clojure.core/defn
        :fixed-arities #{0},
        :doc "docstring",
        :added "1.2",
        :deprecated true}]
     var-definitions)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 2,
        :from user,
        :to clojure.core,
        :name defn,
        :arity 4}]
     var-usages))

  (let [{:keys [:var-definitions]} (analyze "(defn foo \"docstring with\\n \\\"escaping\\\"\" [])")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :end-row 1,
        :end-col 46,
        :ns user,
        :name foo,
        :defined-by clojure.core/defn,
        :fixed-arities #{0},
        :doc "docstring with\n \"escaping\""}]
     var-definitions))  

  (let [{:keys [:var-definitions]} (analyze "(def ^:deprecated x \"docstring\" 1)")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 1,
        :col 1,
        :end-row 1,
        :end-col 35,
        :defined-by clojure.core/def,
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
        (analyze "(ns foo (:require [clojure.string :as string]))
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
        :alias string
        :lang :clj}
       {:filename "<stdin>",
        :row 1,
        :col 20,
        :from foo,
        :to clojure.string,
        :alias string
        :lang :cljs}]
     namespace-usages)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 2,
        :col 19,
        :end-row 2,
        :end-col 42,
        :ns foo,
        :name f,
        :defined-by clojure.core/defn
        :fixed-arities #{0},
        :lang :clj}
       {:filename "<stdin>",
        :row 2,
        :col 19,
        :ns foo,
        :name f,
        :defined-by cljs.core/defn
        :fixed-arities #{0},
        :lang :cljs}]
     var-definitions)
    (assert-submaps
     '[{:filename "<stdin>",
        :row 2,
        :col 31,
        :from foo,
        :to clojure.core,
        :name inc,
        :fixed-arities #{1},
        :arity 3,
        :lang :clj}
       {:name defn,
        :varargs-min-arity 2,
        :lang :clj,
        :filename "<stdin>",
        :from foo,
        :macro true,
        :col 20,
        :arity 3,
        :row 2,
        :to clojure.core}
       {:filename "<stdin>",
        :row 2,
        :col 31,
        :from foo,
        :to cljs.core,
        :name inc,
        :fixed-arities #{1},
        :arity 3,
        :lang :cljs}
       {:name defn,
        :varargs-min-arity 2,
        :lang :cljs,
        :filename "<stdin>",
        :from foo,
        :macro true,
        :col 20,
        :arity 3,
        :row 2,
        :to cljs.core}]
     var-usages))
  (let [{:keys [:var-usages]}
        (analyze "(ns foo)
                  (fn [x] x)
                  (fn* [x] x)
                  (bound-fn [x] x)")]
    (assert-submaps
     '[{:filename "<stdin>",
        :row 2,
        :col 20,
        :name fn,
        :from foo,
        :to clojure.core}
       {:filename "<stdin>",
        :row 3,
        :col 20,
        :name fn*,
        :from foo,
        :to clojure.core}
       {:filename "<stdin>",
        :row 4,
        :col 20,
        :name bound-fn,
        :from foo,
        :to clojure.core}]
     var-usages)))

(deftest hooks-custom-defined-by-test
  (assert-submaps
   '[{:ns user,
      :name foobar,
      :defined-by user/defflow}]
   (:var-definitions
    (analyze "(user/defflow foobar)"
             {:config {:output {:analysis {:keywords true}}
                       :hooks {:__dangerously-allow-string-hooks__ true
                               :analyze-call
                               {'user/defflow
                                (str "(require '[clj-kondo.hooks-api :as api])"
                                     "(fn [{:keys [:node]}]"
                                     "  (let [[test-name] (rest (:children node))"
                                     "       new-node (api/list-node"
                                     "                 [(api/token-node 'def)"
                                     "                  test-name])]"
                                     "   {:node (with-meta new-node (meta node))"
                                     "     :defined-by 'user/defflow}))")}}}}))))

(deftest analysis-alias-test
  (let [{:keys [:var-usages]}
        (analyze "(ns foo (:require [bar :as b] baz))
                  (b/w)
                  (bar/x)
                  b/y
                  bar/z")]
    (assert-submaps
      '[{:to bar :alias b :name w}
        {:to bar :name x}
        {:to bar :alias b :name y}
        {:to bar :name z}]
      var-usages)))

(deftest analysis-arglists-test
  (testing "arglist-strs are present on definitions"
    (let [{:keys [:var-definitions]}
          (analyze "(defn f1 [d] d)
                    (defn f2 ([e] e) ([f f'] f))
                    (defprotocol A (f3 [g] \"doc\") (f4 [h] [i i']))
                    (defrecord A [j k])
                    (defmacro f5 [l m])"
                   {:config {:output {:analysis {:arglists true}}}})]
      (assert-submaps
        '[{:name f1,
           :defined-by clojure.core/defn
           :arglist-strs ["[d]"]}
          {:name f2,
           :defined-by clojure.core/defn
           :arglist-strs ["[e]" "[f f']"]}
          {:name f3,
           :defined-by clojure.core/defprotocol
           :arglist-strs ["[g]"]}
          {}
          {:name f4,
           :defined-by clojure.core/defprotocol
           :arglist-strs ["[h]" "[i i']"]}
          {}
          {:name ->A
           :defined-by clojure.core/defrecord
           :arglist-strs ["[j k]"]}
          {:name map->A
           :defined-by clojure.core/defrecord
           :arglist-strs ["[m]"]}
          {:name f5
           :defined-by clojure.core/defmacro
           :arglist-strs ["[l m]"]}]
        var-definitions))))

(deftest analysis-is-valid-edn-test
  (testing "solution for GH-476, CLJS with string require"
    (let [analysis (analyze "(ns foo (:require [\"@dude\" :as d])) (d/fn-call)")
          analysis-edn (pr-str analysis)]
      (is (edn/read-string analysis-edn)))))

(deftest test-var-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo (:require [clojure.test :as t]))
                  (t/deftest foo)")]
    (assert-submaps
     '[{:filename "<stdin>", :row 2, :col 19, :end-row 2, :end-col 34, :ns foo, :name foo, :fixed-arities #{0},
        :test true :defined-by clojure.test/deftest}]
     var-definitions)))

(deftest schema-var-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo (:require [schema.core :as s]))
                  (s/def bar)
                  (s/defn f1 [d] d)
                  (s/defn f2 ([e] e) ([f f'] f))
                  (s/defrecord A [j k])")]
    (assert-submaps
     '[{:end-row 2, :name-end-col 29, :name-end-row 2, :name-row 2, :ns foo, :name bar, :defined-by schema.core/def, :filename "<stdin>", :col 19, :name-col 26, :end-col 30, :row 2}
       {:fixed-arities #{1}, :end-row 3, :name-end-col 29, :name-end-row 3, :name-row 3, :ns foo, :name f1, :defined-by schema.core/defn, :filename "<stdin>", :col 19, :name-col 27, :end-col 36, :row 3}
       {:fixed-arities #{1 2}, :end-row 4, :name-end-col 29, :name-end-row 4, :name-row 4, :ns foo, :name f2, :defined-by schema.core/defn, :filename "<stdin>", :col 19, :name-col 27, :end-col 49, :row 4}
       {:end-row 5, :name-end-col 33, :name-end-row 5, :name-row 5, :ns foo, :name A, :defined-by schema.core/defrecord, :filename "<stdin>", :col 19, :name-col 32, :end-col 40, :row 5}
       {:fixed-arities #{2}, :end-row 5, :name-end-col 33, :name-end-row 5, :name-row 5, :ns foo, :name ->A, :defined-by schema.core/defrecord, :filename "<stdin>", :col 19, :name-col 32, :end-col 40, :row 5}
       {:fixed-arities #{1}, :end-row 5, :name-end-col 33, :name-end-row 5, :name-row 5, :ns foo, :name map->A, :defined-by schema.core/defrecord, :filename "<stdin>", :col 19, :name-col 32, :end-col 40, :row 5}]
     var-definitions)))

(deftest schema-output-schema-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo (:require [schema.core :as s]))
                  (s/defn f1 :- s/Int [d :- s/Str] d)
                  (s/def f2 :- s/Str \"1\")")]
    (assert-submaps
     '[{:output-schema-str "s/Int" :name f1 :defined-by schema.core/defn}
       {:output-schema-str "s/Str" :name f2 :defined-by schema.core/def}]
     var-definitions)))

(deftest declare-var-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo)
                  (declare bar)")]
    (assert-submaps
     '[{:filename "<stdin>", :row 2, :col 19, :end-row 2, :end-col 32, :ns foo, :name bar,
        :defined-by clojure.core/declare}]
     var-definitions)))

(deftest deftype-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo)
                  (deftype Foo [])")]
    (assert-submaps
     '[{:filename "<stdin>", :row 2, :col 19, :end-row 2, :end-col 35, :ns foo, :name Foo, :defined-by clojure.core/deftype}
       {:filename "<stdin>", :row 2, :col 19, :end-row 2, :end-col 35, :ns foo, :name ->Foo, :fixed-arities #{0}, :defined-by clojure.core/deftype}]
     var-definitions)))

(deftest defprotocol-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo)
                  (defprotocol Foo (foo [_]))")]
    (is (= '#{clojure.core/defprotocol} (set (map :defined-by var-definitions))))))

(deftest export-test
  (let [{:keys [:var-definitions]}
        (analyze "(ns foo)
                  (defn ^:export foo [])")]
    (is (true? (:export (first var-definitions))))))

(deftest nested-libspec-test
  (let [{:keys [:namespace-usages :var-usages]}
        (analyze "(ns foo (:require [clojure [set :refer [union]]])) (union #{1 2 3} #{3 4 5})")]
    (is (= 'clojure.set (:to (first namespace-usages))))
    (is (= 'clojure.set (:to (first var-usages))))))

(deftest standalone-require-test
  (let [{:keys [:namespace-usages :var-usages]}
        (analyze "(require '[clojure [set :refer [union]]])")]
    (is (= 'clojure.set (:to (first namespace-usages))))
    (assert-submaps
      '[{:name union :name-row 1 :name-end-row 1 :name-col 33 :name-end-col 38}
        {:name require :name-row 1 :name-end-row 1 :name-col 2 :name-end-col 9}]
     var-usages)))
