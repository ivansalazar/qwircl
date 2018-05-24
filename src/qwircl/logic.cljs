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

(defn valid-play? [{:keys [grid]
                      {:keys [positions]} :turn
                      {:keys [game-state players]} :game}]
  (let [current-hand (:hand (peek players))
        previous-grid (grid/with-positions grid (pop positions) current-hand)
        new-grid (grid/with-positions grid positions current-hand)
        last-move (peek positions)
        previous-moves (pop positions)
        location (last-move :coordinates)
        tile (-> last-move
                 :hand
                 peek ;; to remove this, you need to change grid/with-positions
                 current-hand)]
    (and 
     (grid/empty-location? location previous-grid)
     (or
      (touches-some-tile? location previous-grid)
      (and (= :initial game-state)
           (empty? previous-moves)))
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
