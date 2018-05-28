(ns qwircl.turn
  (:require [qwircl.logic :as logic]))

(defn- add-move [moves location hand]
  (conj moves {:coordinates location :hand (peek hand)}))

;; push-down automaton for managing state for
;; picking and playing tiles
(defn run
  [{{:keys [turn-state hand moves]} :turn :as state} {:keys [action clicked]}]
  (defmulti => (fn [turn-state action] [turn-state action]))
  (defmethod => :default [_ _]
    {:turn-state turn-state :hand hand :moves moves})
  (defmethod => [:initial :hand-clicked] [_ _]
    {:turn-state :picking :hand [clicked] :moves []})
  (defmethod => [:picking :hand-clicked] [_ _]
    (if (and (empty? moves) (not-any? #(= clicked %) hand))
      {:turn-state :picking :hand (conj hand clicked) :moves []}
      (=> :default)))
  (defmethod => [:picking :trade] [_ _]
    (if (empty? moves)
      {:turn-state :traded :hand hand}
      (=> :default)))
  (defmethod => [:picking :grid-clicked] [_ _]
    (if (and (= 1 (count hand))
             (not-any? #(= clicked (:coordinates %)) moves)
             (logic/valid-play? (assoc-in state
                                          [:turn :moves] 
                                          (add-move moves clicked hand)))) 
      {:turn-state :playing :hand [] :moves (add-move moves clicked hand)}
      (=> :default)))
  (defmethod => [:picking :undo] [_ _]
    (cond
      (and 
       (= 1 (count hand))
       (empty? moves)) {:turn-state :initial}
      (empty? moves) {:turn-state :picking :hand (pop hand) :moves moves}
      :else {:turn-state :playing :hand (pop hand) :moves moves}))
  (defmethod => [:playing :hand-clicked] [_ _]
    (if (not-any? #(= clicked (:hand %)) moves)
      {:turn-state :picking :hand (conj hand clicked) :moves moves}
      (=> :default)))
  (defmethod => [:playing :undo] [_ _]
    (if (= 1 (count moves))
      {:turn-state :initial}
      {:turn-state :playing :hand hand :moves (pop moves)}))
  (defmethod => [:playing :submit] [_ _]
    {:turn-state :played :moves moves})
  (=> turn-state action))
