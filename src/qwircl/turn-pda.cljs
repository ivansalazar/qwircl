(ns qwircl.turn
  (:require [qwircl.logic :as logic]))

(defn update-positions [{{:keys [hand positions]} :turn :as state} clicked]
  (assoc-in state 
            [:turn :positions] 
            (conj positions {:coordinates clicked :hand (peek hand)})))

;; push-down automaton for managing state for
;; picking and playing tiles
(defn run-pda 
  [{{:keys [turn-state hand positions]} :turn :as state} {:keys [action clicked]}]
  (case turn-state
    :initial 
    (cond
      (= action :hand-clicked) {:turn-state :picking :hand [clicked] :positions []}
      :else {:turn-state :initial})
    :picking 
    (cond
      (and 
       (= action :hand-clicked)
       (empty? positions)
       (not-any? #(= clicked %) hand)) {:turn-state :picking 
                                        :hand (conj hand clicked)
                                        :positions []}
      (and
       (= action :trade)
       (empty? positions)) {:turn-state :traded :hand hand}
      (and
       (= action :grid-clicked)
       (= 1 (count hand))
       (not-any? #(= clicked (:coordinates %)) positions)
       (logic/valid-play? 
        (update-positions state clicked))) {:turn-state :playing 
                                            :hand []
                                            :positions (conj positions 
                                                             {:coordinates clicked
                                                              :hand (peek hand)})}
      (= action :undo) (cond
                         (and 
                          (= 1 (count hand))
                          (empty? positions)) {:turn-state :initial}
                         (empty? positions) {:turn-state :picking
                                             :hand (pop hand)
                                             :positions positions}
                         :else {:turn-state :playing :hand (pop hand) :positions positions})
      :else {:turn-state :picking :hand hand :positions positions})
    :playing
    (cond
      (and
       (= action :hand-clicked)
       (not-any? #(= clicked (:hand %)) positions)) {:turn-state :picking 
                                                     :hand (conj hand clicked)
                                                     :positions positions}
      (= action :undo) (if (= 1 (count positions))
                         {:turn-state :initial}
                         {:turn-state :playing :hand hand :positions (pop positions)})
      (= action :submit) {:turn-state :played :positions positions}
      :else {:turn-state :playing :hand hand :positions positions})))
