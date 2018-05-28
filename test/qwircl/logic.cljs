(ns qwircl.logic-test
  (:require
   [qwircl.logic :refer [valid-play?]]
   [cljs.test :refer-macros [deftest is]]))

(def state
  {:grid 
   (-> (vec (repeat 10 (vec (repeat 10 nil))))
       (assoc-in [3 3] {:color :green :shape :cross})
       (assoc-in [4 3] {:color :green :shape :circle})
       (assoc-in [5 3] {:color :green :shape :diamond})
       (assoc-in [3 4] {:color :blue :shape :cross})
       (assoc-in [5 4] {:color :blue :shape :diamond}))
   :game {:players #queue [{:hand [{:color :yellow :shape :cross}
                                   {:color :green :shape :clover}
                                   {:color :red :shape :diamond}
                                   {:color :blue :shape :circle}
                                   {:color :green :shape :cross}
                                   {:color :green :shape :square}
                                   {:color :purple :shape :cross}
                                   {:color :purple :shape :circle}]}]}})

(defn assoc-moves [& pairs]
  (->> pairs
       (map #(into {} [[:coordinates (first %)] 
                       [:hand (second %)]]))
       (into [])
       (assoc-in state [:turn :moves])))

(deftest is-valid-play-single-tile
  (is (valid-play? {:grid (vec (repeat 10 nil))
                    :game {:game-state :initial
                           :players #queue [{:hand [{:color :yellow :shape :cross}]}]}
                    :turn {:moves [{:coordinates [0 0] :hand 0}]}})
      "Opening move doesn't need to touch a tile")
  (is (valid-play? (assoc-moves [[3 5] 0])) "Same shape, below")
  (is (valid-play? (assoc-moves [[2 4] 0])) "Same shape, above")
  (is (valid-play? (assoc-moves [[2 4] 0])) "Same shape, left")
  (is (valid-play? (assoc-moves [[6 4] 2])) "Same shape, right")
  (is (valid-play? (assoc-moves [[2 3] 1])) "Same color")
  (is (valid-play? (assoc-moves [[4 4] 3])) 
      "Touches tiles horizontally and vertically"))

(deftest is-not-valid-play-single-tile
  (is (not (valid-play? {:grid (vec (repeat 10 nil))
                           :game {:game-state :initial
                                  :players #queue [{:hand [{:color :yellow 
                                                            :shape :cross}
                                                           {:color :yellow 
                                                            :shape :clover}]}]}
                         :turn {:moves [{:coordinates [0 0] :hand 0}
                                        {:coordinates [2 0] :hand 1}]}}))
      "Opening move doesn't need to touch tile, but subsequent ones have to")
  (is (not (valid-play? (assoc-moves [[0 0] 0]))) "Doesn't touch a tile")
  (is (not (valid-play? (assoc-moves [[3 3] 0]))) "Is replacing a tile")
  (is (not (valid-play? (assoc-moves [[6 3] 0]))) "Not right shape or color")
  (is (not (valid-play? (assoc-moves [[6 3] 4]))) "Not unique - repeats shape")
  (is (not (valid-play? (assoc-moves [[3 5] 4]))) "Not unique - repeats color"))

(deftest is-valid-play-multiple-tiles
  (is (valid-play? (assoc-moves [[2 3] 5] [[1 3] 1])) 
      "Same horizontal, touching each other")
  (is (valid-play? (assoc-moves [[3 2] 0] [[3 1] 6])) 
      "Same vertical, touching each other")
  (is (valid-play? (assoc-moves [[2 3] 5] [[6 3] 1])) 
      "Same horizontal, not touching each other")
  (is (valid-play? (assoc-moves [[3 2] 0] [[3 5] 6])) 
      "Same vertical, not touching each other")
  (is (valid-play? (assoc-moves [[4 2] 7] [[4 4] 3])) 
      "Tiles touching vertically and horizontally others that were already placed"))

(deftest not-valid-play-multiple-tiles
  (is (not (valid-play? (assoc-moves [[2 3] 1] [[6 4] 3]))) 
      "Not in the same horizontal or vertical")
  (is (not (valid-play? (assoc-moves [[2 3] 1] [[6 3] 5] [[2 2] 4]))) 
      "In both a vertical and horizontal"))
