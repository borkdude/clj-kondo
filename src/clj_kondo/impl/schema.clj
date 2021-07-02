(ns clj-kondo.impl.schema
  {:no-doc true}
  (:require
   [clj-kondo.impl.utils :as utils]))

(defn remove-schemas-from-children [expr]
  (let [children (:children expr)
        {:keys [:new-children :schemas]}
        (loop [[fst-child & rest-children] children
               res {:new-children []
                    :schemas []}]
          (let [sexpr (when fst-child (utils/sexpr fst-child))]
            (cond (not fst-child)
                  res
                  (= ':- (utils/sexpr fst-child))
                  (recur (next rest-children)
                         (update res :schemas conj (first rest-children)))
                  (vector? sexpr)
                  (recur rest-children
                         (let [{:keys [:expr :schemas]} (remove-schemas-from-children fst-child)]
                           (-> res
                               (update :schemas into schemas)
                               (update :new-children conj expr))))
                  :else (recur rest-children
                               (update res :new-children conj fst-child)))))]
    {:expr (assoc expr :children new-children)
     :schemas schemas}))

(defn- defmethod-dispatch-val? [fn-sym index]
  (and (= fn-sym 'defmethod) (= index 2)))

(defn expand-schema
  [_ctx fn-sym expr]
  (let [children (:children expr)
        {:keys [:new-children
                :output-schema
                :schemas]}
        (loop [[fst-child & rest-children] children
               index 0
               res {:new-children []
                    :schemas []
                    :output-schema nil}]
          (let [sexpr (when fst-child (utils/sexpr fst-child))]
            (cond (not fst-child)
                  res
                  (= ':- sexpr)
                  (recur (next rest-children)
                         (inc index)
                         (-> res
                             (update :schemas conj (first rest-children))
                             (assoc :output-schema (first rest-children))))
                  (and (vector? sexpr) (not (defmethod-dispatch-val? fn-sym index)))
                  (let [{:keys [:expr :schemas]} (remove-schemas-from-children fst-child)]
                    (-> res
                        (update :schemas into schemas)
                        (update :new-children conj expr)
                        (update :new-children into rest-children)))
                  (list? sexpr)
                  (recur rest-children
                         (inc index)
                         (let [cchildren (:children fst-child)
                               {:keys [:expr :schemas]} (remove-schemas-from-children (first cchildren))
                               new-cchildren (cons expr (rest cchildren))
                               new-fst-child (assoc fst-child :children new-cchildren)]
                           (-> res
                               (update :schemas into schemas)
                               (update :new-children conj new-fst-child))))
                  :else (recur rest-children
                               (inc index)
                               (update res :new-children conj fst-child)))))
        new-children (if output-schema
                       (assoc-in new-children [1 :output-schema] output-schema)
                       new-children)]
    {:expr (assoc expr :children new-children)
     :schemas schemas}))

;;;; Scratch

(comment)
