(ns qwircl.grid)

(defn with-positions [grid positions hand]
  (reduce #(assoc-in %1 (:coordinates %2) (hand (first (:hand %2)))) grid positions))

(defn with-moves [grid {:keys [coordinates hand]} current-hand] ;; Q: from-hand instead of hand???
  (reduce #(assoc-in %1 (first %2) (current-hand (second %2))) grid (map vector coordinates hand)))

(defn empty-location? [[x y] grid]
  (let [cell (get-in grid [x y])]
    (or
     (nil? (:color cell))
     (nil? (:shape cell)))))

(def transformations {:horizontal [[dec identity] [inc identity]]
                         :vertical [[identity dec] [identity inc]]})
(defn get-neighbors [grid [x y] direction]
   (let [[[fx1 fy1] [fx2 fy2]] (transformations direction)
         f #(juxt (comp %1 first) (comp %2 second))
         f1 (f fx1 fy1)
         f2 (f fx2 fy2)
         h (fn [f]
             (vec 
              (map #(get-in grid %)
                   (take-while #(not (empty-location? % grid))
                               (iterate f (f [x y]))))))]
     (into (h f1) (h f2)))) 

(defn every-neighbor? [pred? grid location direction]
  (->> (get-neighbors grid location direction) 
       (every? pred?)))
