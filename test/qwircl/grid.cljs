(ns qwircl.grid-test
  (:require 
   [qwircl.grid :refer [empty-location? get-neighbors with-moves]]
   [cljs.test :refer-macros [deftest is]]))

(def grid
  (-> (vec (repeat 10 (vec (repeat 10 nil))))
      (assoc-in [3 3] {:color :green :shape :cross})
      (assoc-in [4 3] {:color :green :shape :circle})
      (assoc-in [5 3] {:color :green :shape :diamond})
      (assoc-in [3 4] {:color :blue :shape :cross})
      (assoc-in [5 4] {:color :blue :shape :diamond})))

(deftest with-moves-test
  (let [new-grid (vec (repeat 10 (vec (repeat 10 nil))))]
    (is (=
         (-> new-grid
          (assoc-in [0 0] :a)
          (assoc-in [1 1] :b)
          (assoc-in [2 2] :c))
         (with-moves 
           new-grid 
           [{:coordinates [0 0] :hand 0}
            {:coordinates [1 1] :hand 1}
            {:coordinates [2 2] :hand 2}]
           [:a :b :c])))))

(deftest empty-location-test
  (is (empty-location? [0 0] grid))
  (is (not (empty-location? [3 3] grid))))

(deftest get-neighbors-test
  (is (empty? (get-neighbors grid [0 0] :horizontal)))
  (is (empty? (get-neighbors grid [0 0] :vertical)))
  (let [ns (get-neighbors grid [3 3] :horizontal)]
    (is (= 2 (count ns)))
    (is (and
         (contains? (set ns)
                    {:color :green :shape :circle})
         (contains? (set ns)
                    {:color :green :shape :diamond}))))
  (let [ns (get-neighbors grid [3 3] :vertical)]
    (is (= 1 (count ns)))
    (is (and 
         (contains? (set ns)
                    {:color :blue :shape :cross})))))
