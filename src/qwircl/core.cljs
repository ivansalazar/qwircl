(ns qwircl.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

; (in-ns 'qwircl.core)
; (require '[quil.middleware :as m])
; (require '[quil.core :as q :include-macros true])

(def tiles 25)
(def size 30)
(def header (* 1.5 size))
(def width (* size tiles))
(def height width)
(def background-color [211 211 211])
(def state {:grid (-> (vec (repeat tiles (vec (repeat tiles nil))))
                      (assoc-in [3 3] {:color :green :shape :cross})) 
            :turn :player1
            :player1 {:hand [{:color :green :shape :diamond} 
                             {:color :purple :shape :circle}
                             {:color :yellow :shape :rectangle}
                             {:color :blue :shape :star}
                             {:color :red :shape :clover}
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

(defn click-event [state event]
  ; Hacky demo of updating state based on click coordinates.
  ;; {:color (mod (:x event) 255)
  ;;  :angle (:y event)}
  state)

;; (defn draw-state [state]
;;   ; Clear the sketch by filling it with light-grey color.
;;   (q/background 240)
;;   ; Set circle color.
;;   (q/fill (:color state) 255 255)
;;   ; Calculate x and y coordinates of the circle.
;;   (let [angle (:angle state)
;;         x (* 150 (q/cos angle))
;;         y (* 150 (q/sin angle))]
;;     ; Move origin point to the center of the sketch.
;;     (q/with-translation [(/ (q/width) 2)
;;                          (/ (q/height) 2)]
;;       ; Draw the circle.
;;       (q/ellipse x y 100 100))))

(defn set-color [color]
  (q/fill 
   (condp = color 
     :green [0 120 0]
     :blue [135 206 250]
     :purple [120 0 120]
     :red [120 0 0]
     :orange [255 165 0]
     :yellow [255 255 0]
     :black [0 0 0]
     ; [0 255 255]
     background-color)))

(defn draw-black-background [x y]
  (set-color :black)
  (q/rect (* size x) (* size y) size size))

(defn draw-tile [x y tile]
  (let [middle (/ size 2)
        quarter (* size 0.25)
        eighth (* 0.5 quarter)
        third (/ size 3)]
    (draw-black-background x y)
    (set-color (tile :color))
    (q/no-stroke)
    (q/with-translation [(* size x) (* size y)]
      (condp = (tile :shape)
        :clover (do
                  (q/ellipse middle third quarter quarter)
                  (q/ellipse third middle quarter quarter)
                  (q/ellipse middle (* size (/ 2 3)) quarter quarter)
                  (q/ellipse (* size (/ 2 3)) middle quarter quarter)
                  (q/ellipse middle middle quarter quarter))
        :star (do 
                (q/triangle middle 0 
                            size (* 0.75 size)
                            0 (* 0.75 size))
                (q/triangle middle size
                            0 quarter
                            size quarter))
        :cross (do
                 (q/quad quarter 0
                         (- size quarter) 0
                         (- size quarter) size
                         quarter size)
                 (q/quad 0 quarter
                         0 (- size quarter)
                         size (- size quarter)
                         size quarter))
        :circle (q/ellipse middle middle (* 0.75 size) (* 0.75 size))
        :diamond (q/quad middle quarter 
                         (- size quarter) middle
                         middle (- size quarter)
                         quarter middle)
        :rectangle (q/rect quarter quarter middle middle)))
    (q/no-stroke)
    (set-color :background)))

(defn draw-empty-space [x y]
  (set-color :background)
  (q/rect (* size x) (* size y) size size))

(defn draw-grid [grid]
  (q/with-translation [0 header]
    (doseq [x (range tiles) 
            y (range tiles)]
      (if-let [tile (get-in grid [x y])]
        (draw-tile x y tile)
        (draw-empty-space x y)))))

(defn draw-header [player]
  (set-color :black)
  (q/text (:name player) 10 header)
  (dotimes [x (count (:hand player))]
    (draw-tile x 0 ((:hand player) x)))
  (set-color :background))

(defn draw-state [state]
  (q/frame-rate 120)
  (q/background 240)
  (set-color :background)
  (q/no-stroke)
  (draw-header ((:turn state) state))
  (draw-grid (:grid state)))

; hacky way of replacing the default cljs.main page with a canvas for quil
(def ^:const canvas-id "sketch")
(def ^:const canvas (.createElement js/document "canvas"))
(.setAttribute canvas "id" canvas-id)
(.appendChild (.-body js/document) canvas)
(.removeChild (.-body js/document) (.querySelector js/document "#app"))

; start quil
(q/defsketch hello-world
    :host canvas-id
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
    :middleware [m/fun-mode])
