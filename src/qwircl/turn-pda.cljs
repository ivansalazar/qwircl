(ns qwircl.turn
  (:require [qwircl.logic :as logic]))

(defn add-move [moves location hand]
  (conj moves {:coordinates location :hand (peek hand)}))

;; push-down automaton for managing state for
;; picking and playing tiles
(defn run-pda 
  [{{:keys [turn-state hand moves]} :turn :as state} {:keys [action clicked]}]
  (case turn-state
    :initial 
    (cond
      (= action :hand-clicked) {:turn-state :picking :hand [clicked] :moves []}
      :else {:turn-state :initial})
    :picking 
    (cond
      (and 
       (= action :hand-clicked)
       (empty? moves)
       (not-any? #(= clicked %) hand)) {:turn-state :picking 
                                        :hand (conj hand clicked)
                                        :moves []}
      (and
       (= action :trade)
       (empty? moves)) {:turn-state :traded :hand hand}
      (and
       (= action :grid-clicked)
       (= 1 (count hand))
       (not-any? #(= clicked (:coordinates %)) moves)
       (logic/valid-play? (assoc-in state [:turn :moves] (add-move moves clicked hand)))) 
      {:turn-state :playing :hand [] :moves (add-move moves clicked hand)}
      (= action :undo) (cond
                         (and 
                          (= 1 (count hand))
                          (empty? moves)) {:turn-state :initial}
                         (empty? moves) {:turn-state :picking
                                         :hand (pop hand)
                                         :moves moves}
                         :else {:turn-state :playing :hand (pop hand) :moves moves})
      :else {:turn-state :picking :hand hand :moves moves})
    :playing
    (cond
      (and
       (= action :hand-clicked)
       (not-any? #(= clicked (:hand %)) moves)) {:turn-state :picking 
                                                 :hand (conj hand clicked)
                                                 :moves moves}
      (= action :undo) (if (= 1 (count moves))
                         {:turn-state :initial}
                         {:turn-state :playing :hand hand :moves (pop moves)})
      (= action :submit) {:turn-state :played :moves moves}
      :else {:turn-state :playing :hand hand :moves moves})))
