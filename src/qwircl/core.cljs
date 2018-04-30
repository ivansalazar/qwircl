(ns qwircl.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.string :as s]))

(def tiles 25)
(def size 30)
(def header (* 1.8 size))
(def header-translation [10 11])
(def width (* size tiles))
(def height width)
(def background-color [211 211 211])
(def state {:grid (-> (vec (repeat tiles (vec (repeat tiles nil))))
                      (assoc-in [3 3] {:color :green :shape :cross})) 
            :turn :player1
            :pda {:s :initial} 
            :my {:hand [{:color :green :shape :diamond} 
                        {:color :purple :shape :circle}
                        {:color :red :shape :clover}
                        {:color :orange :shape :cross}]
                 :name "Name"}})

(defn setup []
  state)

(defn update-state [state]
  ; nothing yet
  state)

(defn inside-dimensions? [xp yp {:keys [x y w h]}]
  (and 
   (<= x xp (+ x w))
   (<= y yp (+ y h))))

;; These represent the dimensions of the different UI components 
;; that are clickable or relevant in some other way in the sketch.
;; All of them are constant except for the [:hand :w] dimension
;; which will change depending on how many tiles are in the actual hand.
;; This check will be deferred to the translate-hand function where the
;; state (and the hand) will be available.
(def dimensions 
  {:hand {:x (first header-translation) 
          :w (* 6 size) 
          :y (second header-translation) 
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
(defn get-clicked [x y]
  (first (filter #(inside-dimensions? x y (% dimensions)) [:hand :grid :submit :undo])))

(defn translate-hand [xp hand]
  (let [x (int (/ (- xp (first header-translation)) size))]
    (when (and (<= 0 x) (< x (count hand)))
      [x])))

(defn translate-grid [x y]
  [(int (/ x size)) (int (/ (- y header) size))])

(defn translate-event [state {:keys [x y] :as event}]
  (condp = (get-clicked x y)
    :hand (when-let [h (translate-hand x (get-in state [:my :hand]))] 
            {:action :hand-clicked :clicked h})
    :grid {:action :grid-clicked :clicked (translate-grid x y)}
    :submit {:action :submit}
    :undo {:action :undo}
    event))

(defn empty-location? [[x y] grid]
  (let [location (get-in grid [x y])]
    (and 
     (nil? (:color location))
     (nil? (:shape location)))))

(defn valid-play? [state clicked]
  (and 
   (empty-location? clicked (:grid state))
   true))

;; push-down automaton for managing state for
;; picking and playing tiles
(defn run-pda 
  [{{:keys [s hand positions] :as pda} :pda :as state} {:keys [action clicked]}]
  (condp = s
    :initial 
    (cond
      (= action :hand-clicked) {:s :picking :hand [clicked] :positions []}
      :else {:s :initial})
    :picking 
    (cond
      (and 
       (= action :hand-clicked)
       (empty? positions)
       (not-any? #(= clicked %) hand)) {:s :picking 
                                        :hand (conj hand clicked)
                                        :positions []}
      (and
       (= action :trade-clicked)
       (empty? positions)) {:s :traded :hand hand}
      (and
       (= action :grid-clicked)
       (= 1 (count hand))
       (not-any? #(= clicked (:coordinates %)) positions)
       (valid-play? state clicked)) {:s :playing 
                                     :hand []
                                     :positions (conj positions {:coordinates clicked
                                                                 :hand (peek hand)})}
      (= action :undo) (cond
                         (and 
                          (= 1 (count hand))
                          (empty? positions)) {:s :initial}
                         :else {:s :playing :hand (pop hand) :positions positions})
      :else {:s :picking :hand hand :positions positions})
    :playing
    (cond
      (and
       (= action :hand-clicked)
       (not-any? #(= clicked (:hand %)) positions)) {:s :picking 
                                                     :hand (conj hand clicked)
                                                     :positions positions}
      (= action :play-clicked) {:s :played :positions positions}
      (= action :undo) (if (= 1 (count positions))
                         {:s :initial}
                         {:s :playing :hand hand :positions (pop positions)})
      :else {:s :playing :hand hand :positions positions})))

(defn click-event [state event]
  (let [click (translate-event state event)]
    (-> state
        (assoc :pda (run-pda state click)))))

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

(defn button-status [button pda]
  (condp = (:s pda)
    :initial :inactive 
    :playing :active
    :picking (condp = button 
               :submit :active
               :undo :active
               :inactive)))

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

(defn draw-header [{:keys [pda] {:keys [name hand]} :my :as state}]
  (draw-button :submit 232 pda)
  (draw-button :undo 340 pda)
  (set-color :black)
  (q/text (str "Playing: " name
               ; " debug: " (get-in state [:debug])
               )
          415 30)
  (q/with-translation header-translation
    (dotimes [x (count hand)]
      (draw-tile x 0 (get hand x))))
  (set-color :background))

(defn draw-pda [{{:keys [s hand positions]} :pda :as state}] 
  (q/with-translation header-translation
    (doseq [[h] hand]
      (draw-empty-space h 0)
      (as-> state arg
        (get-in arg [:my :hand h])
        (assoc arg :highlighted? true)
        (draw-tile h 0 arg)))
    (doseq [{[h] :hand} positions]
      (draw-empty-space h 0)))
  (q/with-translation [0 header]
    (doseq [{[x y] :coordinates [h] :hand} positions]
      (let [tile (get-in state [:my :hand h])]
        (draw-tile x y (assoc tile :highlighted? true))))))

(defn draw-state [state]
  (q/background 240)
  (set-color :background)
  (q/no-stroke)
  (draw-header state)
  (draw-grid (:grid state))
  (draw-pda state))

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
