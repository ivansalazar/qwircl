(ns qwircl.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

; (in-ns 'qwircl.core)
; (require '[quil.middleware :as m])
; (require '[quil.core :as q :include-macros true])

(def tiles 25)
(def size 30)
(def header (* 2 size))
(def width (* size tiles))
(def height width)
(def background-color [211 211 211])
(def state {:grid (-> (vec (repeat tiles (vec (repeat tiles nil))))
                      (assoc-in [3 3] {:color :green :shape :cross :highlighted? true})) 
            :turn :player1
            :player1 {:hand [{:color :green :shape :diamond} 
                             {:color :purple :shape :circle}
                             {:color :yellow :shape :rectangle :highlighted? true}
                             {:color :blue :shape :star}
                             {:color :red :shape :clover :highlighted? true}
                             {:color :orange :shape :cross}]
                      :name "Name"}})

(defn setup []
  ; Initial state. It contains
  ; the grid of tiles with an example and
  ; just one player with some tiles in their hand
  state)

(defn update-state [state]
  ; nothing yet
  state)

(defn to-position [event]
  ; x is easy, but for y we need to take the header
  ; into account
  (let [x (int (/ (:x event) size))
        r (:y event)]
    (if (<= r header)
      {:in :hand :coordinates [x]}
      {:in :grid :coordinates [x (int (/ (- r header) size))]}))) 

(defn click-event [state event]
  (let [{:keys [in coordinates]} (to-position event)
        player (:turn state)]
    (let [hand (get-in state [player :hand])
          [x y] coordinates]
      (if (= in :hand)
        (update-in state [player :hand x :highlighted?] not)
        (update-in state [:grid x y :highlighted?] not)))))

(defn get-color [color]
   (condp = color 
     :green [0 120 0]
     :blue [135 206 250]
     :purple [120 0 120]
     :red [120 0 0]
     :orange [255 165 0]
     :yellow [255 255 0]
     :black [0 0 0]
     :pink [255 192 203]
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

(defn draw-header [player]
  (set-color :black)
  (q/text (:name player) 10 (- header 10))
  (dotimes [x (count (:hand player))]
    (draw-tile x 0 ((:hand player) x)))
  (set-color :background))

(defn draw-state [state]
  (q/background 240)
  (set-color :background)
  (q/no-stroke)
  (draw-header ((:turn state) state))
  (draw-grid (:grid state)))

; this function is called in resources/public/index.html
(defn ^:export run-sketch []
  (q/defsketch qwircl
    :host "qwircl"
    :size [width (+ header height)]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-clicked click-event
    :mouse-dragged nil
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
