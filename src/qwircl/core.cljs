(ns qwircl.core
  (:require [qwircl.turn :as turn]
            [qwircl.ui.core :as ui]
            [qwircl.ui.handlers :as ui-handlers]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  {:grid (-> (vec (repeat ui/tiles (vec (repeat ui/tiles nil))))
                      (assoc-in [3 3] {:color :green :shape :cross})
                      (assoc-in [4 3] {:color :green :shape :circle})
                      (assoc-in [5 3] {:color :green :shape :diamond})
                      (assoc-in [3 4] {:color :blue :shape :cross})
                      (assoc-in [5 4] {:color :blue :shape :diamond})) 
            :turn :player1
            :pda {:s :initial} 
            :my {:hand [{:color :blue :shape :diamond} 
                        {:color :blue :shape :circle}
                        {:color :blue :shape :clover}
                        {:color :blue :shape :cross}
                        {:color :orange :shape :cross}]
                 :name "Name"}})

(defn update-state [state]
  state)

(defn click-event [state event]
  (let [click (ui-handlers/translate-event state event)]
    (-> state
        (assoc :pda (turn/run-pda state click))
        (assoc :debug click))))

; this function is called in resources/public/index.html
(defn ^:export run-sketch []
  (q/defsketch qwircl
    :host "qwircl"
    :size [ui/width (+ ui/header ui/height)]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw ui/draw-state
    :mouse-clicked click-event
     ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
