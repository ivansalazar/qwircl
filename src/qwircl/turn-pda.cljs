(ns qwircl.turn
  (:require [qwircl.logic :as logic]))

;; push-down automaton for managing state for
;; picking and playing tiles
(defn run-pda 
  [{{:keys [s hand positions]} :turn :as state} {:keys [action clicked]}]
  (case s
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
       (logic/valid-play? state clicked)) {:s :playing 
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
