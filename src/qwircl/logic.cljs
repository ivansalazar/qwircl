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

(defn all-unique? [grid location tile direction]
  (let [ts (conj (grid/get-neighbors grid location direction) tile)]
    (= (count ts)
       (count (set ts)))))

(defn same-shape? [grid location tile direction]
  (grid/every-neighbor? #(= (:shape tile) (:shape %)) grid location direction))

(defn same-color? [grid location tile direction]
  (grid/every-neighbor? #(= (:color tile) (:color %)) grid location direction))

(defn valid-play? 
  [{:keys [grid] {:keys [positions hand]} :turn {my-hand :hand} :my} location]
  (let [previous-grid (grid/with-positions grid positions my-hand)
        new-grid (grid/with-positions 
                   grid 
                   (conj positions {:coordinates location
                                    :hand (peek hand)})
                   my-hand)
        tile (my-hand (first (peek hand)))]
    (and 
     (grid/empty-location? location previous-grid)
     (touches-some-tile? location previous-grid)
     (same-line? positions location)
     (and 
      (or 
       (same-shape? new-grid location tile :vertical)
       (same-color? new-grid location tile :vertical))
      (or 
       (same-shape? new-grid location tile :horizontal)
       (same-color? new-grid location tile :horizontal)))
     (all-unique? new-grid location tile :horizontal)
     (all-unique? new-grid location tile :vertical))))
