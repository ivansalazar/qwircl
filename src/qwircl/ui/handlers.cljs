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
  (let [x (int (/ (- xp (get-in ui/dimensions [:hand :x])) ui/size))]
    (when (and (<= 0 x) (< x (count hand)))
      [x])))

(defn translate-grid [x y]
  [(int (/ x ui/size)) (int (/ (- y ui/header) ui/size))])

(defn translate-event 
  [{:keys [turn] {:keys [players] :as game} :game} {:keys [x y] :as event}]
  (let [current-hand (:hand (peek players))]
    (case (get-clicked x y)
      :hand (when-let [h (translate-hand x current-hand)] 
              {:action :hand-clicked :clicked h})
      :grid {:action :grid-clicked :clicked (translate-grid x y)}
      :submit {:action (ui/button-action :submit turn)
               :status (ui/button-status :submit turn game)}
      :undo {:action :undo
             :status (ui/button-status :undo turn game)}
      event)))
