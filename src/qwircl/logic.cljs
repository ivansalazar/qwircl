(ns qwircl.logic
  (:require [qwircl.grid :as grid]))

(defn touches-some-tile? [[x y] grid]
  (let [uc [x (dec y)]
        dc [x (inc y)]
        lc [(dec x) y]
        rc [(inc x) y]]
    (some #(get-in grid %) [uc dc lc rc])))

(defn same-line? [positions [x y]]
  (or (empty? positions)
      (every? #(= (first (:coordinates %)) x)
              positions)
      (every? #(= (second (:coordinates %)) y) 
              positions)))

(defn all-unique? [grid clicked tile direction]
  (let [ts (conj (grid/get-neighbors grid clicked direction) tile)]
    (= (count ts)
       (count (set ts)))))

(defn same-shape? [grid clicked tile direction]
  (grid/every-neighbor? #(= (:shape tile) (:shape %)) grid clicked direction))

(defn same-color? [grid clicked tile direction]
  (grid/every-neighbor? #(= (:color tile) (:color %)) grid clicked direction))

(defn valid-play? 
  [{:keys [grid] {:keys [positions hand]} :pda {my-hand :hand} :my} clicked]
  (let [previous-grid (grid/with-positions grid positions my-hand)
        new-grid (grid/with-positions 
                   grid 
                   (conj positions {:coordinates clicked
                                    :hand (peek hand)})
                   my-hand)
        tile (my-hand (first (peek hand)))]
    (and 
     (grid/empty-location? clicked previous-grid)
     (touches-some-tile? clicked previous-grid)
     (same-line? positions clicked)
     (and 
      (or 
       (same-shape? new-grid clicked tile :vertical)
       (same-color? new-grid clicked tile :vertical))
      (or 
       (same-shape? new-grid clicked tile :horizontal)
       (same-color? new-grid clicked tile :horizontal)))
     (all-unique? new-grid clicked tile :horizontal)
     (all-unique? new-grid clicked tile :vertical))))
