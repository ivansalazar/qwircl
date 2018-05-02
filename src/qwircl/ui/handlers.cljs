(ns qwircl.ui.handlers
  (:require [qwircl.ui.core :as ui]))

(defn inside-dimensions? [xp yp {:keys [x y w h]}]
  (and 
   (<= x xp (+ x w))
   (<= y yp (+ y h))))

(defn get-clicked [x y]
  (first (filter #(inside-dimensions? x y (% ui/dimensions)) 
                 [:hand :grid :submit :undo])))

(defn translate-hand [xp hand]
  (let [x (int (/ (- xp (:x ui/dimensions)) ui/size))]
    (when (and (<= 0 x) (< x (count hand)))
      [x])))

(defn translate-grid [x y]
  [(int (/ x ui/size)) (int (/ (- y ui/header) ui/size))])

(defn translate-event [{{my-hand :hand} :my} {:keys [x y] :as event}]
  (condp = (get-clicked x y)
    :hand (when-let [h (translate-hand x my-hand)] 
            {:action :hand-clicked :clicked h})
    :grid {:action :grid-clicked :clicked (translate-grid x y)}
    :submit {:action :submit}
    :undo {:action :undo}
    event))
