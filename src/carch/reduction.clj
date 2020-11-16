(ns carch.reduction
  (:require [clojure.test :refer :all])
  (:import [clojure.lang IReduceInit IReduce]))

(defn process! [reducible & transducers]
  (transduce (apply comp transducers)
             (constantly nil)
             reducible))

(defmacro do-reducible [binding & body]
  (let [[variable reducible] binding]
    `(reduce (fn [result_# ~variable]
               ~@body
               nil)
             nil
             ~reducible)))

(defn educe [reducible & transducers]
  (eduction (apply comp transducers)
            reducible))

(deftest test-educe
  (is (= [1 2]
         (into []
               (educe (range 10)
                      (map inc)
                      (take 2))))))

(defn reduce-tree [root children reducing-function initial-value]
  (loop [reduced-value initial-value
         nodes [root]]
    (if-let [node (first nodes)]
      (if-let [the-children (children node)]
        (recur reduced-value
               (concat the-children
                       (rest nodes)))
        (let [reusult (reducing-function reduced-value
                                         node)]
          (if (reduced? reusult)
            @reusult
            (recur reusult
                   (rest nodes)))))
      reduced-value)))

(defn tree-reducible [root children]
  (reify
    IReduceInit
    (reduce [this reducing-function initial-reduced-value]
      (reducing-function (reduce-tree root
                                      children
                                      reducing-function
                                      initial-reduced-value)))

    IReduce
    (reduce [this reducing-function]
      (reducing-function (reduce-tree root
                                      children
                                      reducing-function
                                      (reducing-function))))))

(deftest test-tree-reducible
  (is (= [2 3 4]
         (into []
               (map inc)
               (tree-reducible {:a {:b 1
                                    :c 2}
                                :d 3}
                               (fn [value]
                                 (when (map? value)
                                   (vals value))))))))
