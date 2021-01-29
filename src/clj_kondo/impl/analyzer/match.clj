(ns clj-kondo.impl.analyzer.match
  (:require [clj-kondo.impl.analyzer.common :as common]
            [clj-kondo.impl.utils :as utils]))

(defn analyze-token [ctx expr]
  (if (utils/symbol-token? expr)
    (common/extract-bindings ctx expr)
    (do (common/analyze-expression** ctx expr)
        nil)))

(declare analyze-expr)

(defn analyze-children [ctx expr]
  (let [children (:children expr)]
    (loop [children (seq children)
           bindings {}]
      (if children
        (let [child (first children)]
          (if-let [bnds (analyze-expr ctx child)]
            (recur (next children)
                   (into bindings bnds))
            (recur (next children) bindings)))
        bindings))))

(defn analyze-expr [ctx expr]
  (let [tag (utils/tag expr)]
    (case tag
      :token
      (analyze-token ctx expr)
      (:list :vector :map)
      (analyze-children ctx expr)
      ;; default
      (do (common/analyze-expression** ctx expr)
          nil))))

(defn analyze-match [ctx expr]
  (let [[_match pattern & clauses] (:children expr)]
    (common/analyze-expression** ctx pattern)
    (doseq [[clause ret] (partition 2 clauses)]
      (let [bindings (analyze-expr ctx clause)
            ctx (if bindings
                  (utils/ctx-with-bindings ctx bindings)
                  ctx)]
        (common/analyze-expression** ctx ret)))))

;; TODO:
;; or:
#_(let [x 4 y 6 z 9]
  (match [x y z]
         [(:or 1 2 3) _ _] :a0
         [4 (:or 5 6 7) _] :a1
         :else []))
;; defpred
;; (defpred even?)
;; (defpred odd?)
;; (defpred div3?)

;; (deftest guard-pattern-match-1
;;   (is (= (let [y '(2 3 4 5)]
;;            (match [y]
;;                   [([_ (a :when even?) _ _] :seq)] :a0
;;                   [([_ (b :when [odd? div3?]) _ _] :seq)] :a1
;;                   :else []))
;;          :a1)))