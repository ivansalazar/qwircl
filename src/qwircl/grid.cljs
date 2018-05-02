(ns qwircl.grid)

(defn with-positions [grid positions hand]
  (reduce #(assoc-in %1 (:coordinates %2) (hand (first (:hand %2)))) grid positions))

(def transformations {:horizontal {:x [dec inc] :y [identity identity]}
                      :vertical {:x [identity identity] :y [dec inc]}})
(defn get-neighbors 
  ([grid [x y] f g initial]
   (loop [i (f x)
          j (g y)
          acc initial]
     (if-let [cell (get-in grid [i j])]
       (recur (f i) (g j) (conj acc cell))
       acc)))
  ([grid clicked direction]
   (let [[f1 f2 g1 g2] (map #(get-in transformations (apply (partial conj [direction]) %))
                            [[:x 0] [:x 1] [:y 0] [:y 1]])]
     (get-neighbors grid clicked f1 g1  
                    (get-neighbors grid clicked f2 g2 [])))))

(defn empty-location? [[x y] grid]
  (let [location (get-in grid [x y])]
    (or
     (nil? (:color location))
     (nil? (:shape location)))))

;; TODO: only use clicked where appropriate, otherwise use "coordinate"
;; same for all files
(defn every-neighbor? [pred? grid clicked direction]
  (->> (get-neighbors grid clicked direction)
       (every? pred?)))
