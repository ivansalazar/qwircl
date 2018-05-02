(ns qwircl.ui.core
  (:require [quil.core :as q :include-macros true]
            [clojure.string :as s]))

(def tiles 25)
(def size 30)
(def width (* 2 size tiles))
(def height width)
(def header (* 1.8 size))

;; These represent the dimensions of the different UI components 
;; that are clickable or relevant in some other way in the sketch.
;; All of them are constant except for the width of the actual hand
;; which will change depending on how many tiles are in it.
;; This check will be deferred to the translate-hand function where the
;; state (and the hand) will be available.
(def dimensions 
  {:hand {:x 10
          :w (* 6 size) 
          :y 11
          :h size}
   :grid {:x 0 :w width :y header :h height}
   :submit {:x (+ 26 (* 6 size))
            :w 90
            :y 12
            :h 30
            :r 8}
   :undo {:x (+ 130 (* 6 size))
          :w 90
          :y 12
          :h 30
          :r 8}})

(def background-color [211 211 211])
(defn get-color [color]
   (condp = color 
     :white [255 255 255]
     :light-grey [220 220 220]
     :green [0 120 0]
     :light-green [199 234 70]
     :blue [135 206 250]
     :purple [120 0 120]
     :red [120 0 0]
     :orange [255 165 0]
     :yellow [255 255 0]
     :black [0 0 0]
     :pink [255 192 203]
     :background background-color
     background-color))

(defn set-color [color]
  (apply q/fill (get-color color)))

(defn draw-background [x y highlighted?]
  (if highlighted?
    (do 
      (set-color :background)
      (q/stroke-weight 2)
      (apply q/stroke (get-color :red))
      (q/rect (* size x) (* size y) size size 8)
      (q/no-stroke))
    (do 
      (set-color :black)
      (q/rect (* size x) (* size y) size size))))

(defn draw-tile [x y tile]
  (let [half (/ size 2)
        quarter (* size 0.25)
        eighth (* 0.5 quarter)
        third (/ size 3)]
    (draw-background x y (:highlighted? tile))
    (set-color (tile :color))
    (q/no-stroke)
    (q/with-translation [(* size x) (* size y)]
      (condp = (tile :shape)
        :clover (do
                  (q/ellipse half third quarter quarter)
                  (q/ellipse third half quarter quarter)
                  (q/ellipse half (* size (/ 2 3)) quarter quarter)
                  (q/ellipse (* size (/ 2 3)) half quarter quarter)
                  (q/ellipse half half quarter quarter))
        :star (do 
                (q/triangle half 0 
                            size (* 0.75 size)
                            0 (* 0.75 size))
                (q/triangle half size
                            0 quarter
                            size quarter))
        :cross (do
                 (q/rect (- half eighth) eighth quarter (- size quarter))
                 (q/rect eighth (- half eighth) (- size quarter) quarter))
        :circle (q/ellipse half half (* 0.75 size) (* 0.75 size))
        :diamond (q/quad half quarter 
                         (- size quarter) half
                         half (- size quarter)
                         quarter half)
        :rectangle (q/rect quarter quarter half half)))
    (q/no-stroke)
    (set-color :background)))

(defn button-status [button pda]
  (condp = (:s pda)
    :initial :inactive 
    :playing :active
    :picking (condp = button 
               :submit :active
               :undo :active
               :inactive)))

(defn button-action [button pda]
  (condp = button
    :undo :undo
    :submit (condp = (:s pda)
              :picking (if (empty? (:positions pda)) 
                         :trade
                         :submit)
              :playing :submit
              :submit)
    :submit))

(def button-color
  {:inactive :background
   :active :light-green})
(defn draw-button [button x pda]
  (let [status (button-status button pda)]
    (set-color (button-color status))
    (apply q/rect 
           ((juxt :x :y :w :h :r) (dimensions button)))
    (set-color (condp = status
                 :inactive :white
                 :active :black))
    (-> (button-action button pda)
        name
        s/capitalize
        (q/text x 31))))

(defn draw-header [{:keys [pda debug] {:keys [name hand]} :my :as state}]
  (draw-button :submit 232 pda)
  (draw-button :undo 340 pda)
  (set-color :black)
  (q/text (str "Playing: " name
               (if debug 
                 (str  " debug: " debug)
                 ""))
          415 30)
  (q/with-translation ((juxt :x :y) (:hand dimensions))
    (dotimes [x (count hand)]
      (draw-tile x 0 (get hand x))))
  (set-color :background))

(defn draw-empty-space 
  ([x y]
   (set-color :background)
   (q/rect (* size x) (* size y) size size))
  ([x y highlighted]
   (if highlighted
     (draw-background x y true)
     (draw-empty-space x y))))

(defn draw-grid [grid]
  (q/with-translation [0 header]
    (doseq [x (range tiles) 
            y (range tiles)]
      (if-let [cell (get-in grid [x y])]
        ; the cell could be a tile if it has a shape, draw it 
        ; if there's no shape, then the cell might be highlighted
        (if (:shape cell)
          (draw-tile x y cell)
          (draw-empty-space x y (:highlighted? cell)))
        (draw-empty-space x y)))))

(defn draw-pda [{{my-hand :hand} :my {:keys [s hand positions]} :pda :as state}] 
  (q/with-translation ((juxt :x :y) (:hand dimensions))
    (doseq [[h] hand]
      (draw-empty-space h 0)
      (as-> (my-hand h) arg
        (assoc arg :highlighted? true)
        (draw-tile h 0 arg)))
    (doseq [{[h] :hand} positions]
      (draw-empty-space h 0)))
  (q/with-translation [0 header]
    (doseq [{[x y] :coordinates [h] :hand} positions]
      (let [tile (my-hand h)]
        (draw-tile x y (assoc tile :highlighted? true))))))

(defn draw-state [state]
  (q/background 240)
  (set-color :background)
  (q/no-stroke)
  (draw-header state)
  (draw-grid (:grid state))
  (draw-pda state))

