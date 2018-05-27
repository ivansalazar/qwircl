(ns qwircl.core
  (:require [qwircl.turn :as turn]
            [qwircl.ui.core :as ui]
            [qwircl.ui.handlers :as ui-handlers]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 15)
  {:grid (-> (vec (repeat ui/side (vec (repeat ui/side nil))))) 
   :turn {:turn-state :initial} 
   :game {:game-state :initial 
          :players (conj #queue [{:hand [{:color :blue :shape :diamond} 
                                         {:color :blue :shape :circle}
                                         {:color :blue :shape :clover}
                                         {:color :blue :shape :cross}
                                         {:color :orange :shape :cross}]
                                  :id :a
                                  :name "NameA"}
                                 {:hand [{:color :blue :shape :diamond} 
                                         {:color :blue :shape :circle}
                                         {:color :blue :shape :clover}
                                         {:color :blue :shape :cross}
                                         {:color :orange :shape :cross}]
                                  :id :b
                                  :name "NameB"}
                                 {:hand [{:color :blue :shape :diamond} 
                                         {:color :blue :shape :circle}
                                         {:color :blue :shape :clover}
                                         {:color :blue :shape :cross}
                                         {:color :orange :shape :cross}]
                                  :id :c
                                  :name "NameC"}])}})

;; ignores events that come from :inactive ui elements
;; but runs the player turn otherwise
(defn player-turn [state click]
  (if (= :inactive (:status click))
    (:turn state)
    (turn/run state click)))

(defn click-event [state event]
  (let [click (ui-handlers/translate-event state event)]
    (-> state
        (assoc :turn (player-turn state click))
        (assoc :debug {:clicked click 
                       :turn (player-turn state click)
                       :game-state (get-in state [:game :game-state])}))))

; this function is called in resources/public/index.html
(defn ^:export run-sketch []
  (q/defsketch qwircl
    :host "qwircl"
    :size [ui/width (+ ui/header ui/height)]
    ; setup function called only once, during sketch initialization.
    :setup setup
    :draw ui/draw-state
    :mouse-clicked click-event
     ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
