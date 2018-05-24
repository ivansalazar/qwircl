(ns ^:figwheel-always qwircl.test-runner
  (:require 
   [cljs.test :as test :include-macros true :refer [report]]
   [qwircl.core-test]
   [figwheel.client :as fw]))

;; Runner code derived from
;; https://github.com/bhauman/crashverse/blob/master/test/crashverse/test_runner.cljs
;; To run both tests and the figwheel server do:
;; lein figwheel development test
;; Then open localhost:3449 or localhost:3449/test.html as needed

(enable-console-print!)

(defn color-favicon-data-url [color]
  (let [cvs (.createElement js/document "canvas")]
    (set! (.-width cvs) 16)
    (set! (.-height cvs) 16)
    (let [ctx (.getContext cvs "2d")]
      (set! (.-fillStyle ctx) color)
      (.fillRect ctx 0 0 16 16))
    (.toDataURL cvs)))

(defn change-favicon-to-color [color]
  (let [icon (.getElementById js/document "favicon")]
    (set! (.-href icon) (color-favicon-data-url color))))

(defmethod report [::test/default :summary] [m]
  (println "\nRan" (:test m) "tests containing"
           (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors.")
  (if (< 0 (+ (:fail m) (:error m)))
    (change-favicon-to-color "#d00")  
    (change-favicon-to-color "#0d0"))
  (println "\n#############################"))

(defn runner []
 ;  (test/run-all-tests)
 (test/run-all-tests #"qwircl.*")
 ;  (test/run-tests 'qwircl.core-test)
  )

(fw/start {
           :websocket-url "ws://localhost:3449/figwheel-ws"
           :build-id "test"
           :on-jsload (fn [] (runner))})
