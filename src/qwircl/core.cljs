(ns qwircl.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def width 500)
(def height 500)
(def tiles 25)
(def size 20)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; the empty grid of tiles
  (vec (repeat tiles (vec (repeat tiles nil)))))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  ;; {:color (mod (+ (:color state) 0.7) 255)
  ;;  :angle (+ (:angle state) 0.1)}
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

(defn draw-state [state]
  (q/background 240)
  (q/fill 0 255 255)
  (q/stroke 0 0 0)
  (doseq [x (range tiles) 
          y (range tiles)]
    (q/rect (* size x) (* size y) size size)))

; hacky way of replacing the default cljs.main page with a canvas for quil
(def ^:const canvas-id "sketch")
(def ^:const canvas (.createElement js/document "canvas"))
(.setAttribute canvas "id" canvas-id)
(.appendChild (.-body js/document) canvas)
(.removeChild (.-body js/document) (.querySelector js/document "#app"))

; start quil
(q/defsketch hello-world
    :host canvas-id
    :size [width height]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-clicked click-event
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
