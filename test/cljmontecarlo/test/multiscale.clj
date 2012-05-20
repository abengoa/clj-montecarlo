(ns cljmontecarlo.test.multiscale
  (:use [cljmontecarlo.multiscale])
  (:use [clojure.test])
  (:require 
		[clojure.math.numeric-tower :as math1]
		[clojure.contrib.generic.math-functions :as math2]))

(def limits [[-1.0 1.0] [-1.0 1.0]])
  
(defn simple [x y] (+ (* x x) (* y y)))

;; x+y+x^2+y^2+3^sin(x^5+y^6)
(defn complex [x y] (+ x y (* x x) (* y y) (math1/expt 3 (math2/sin (+ (math1/expt x 5) (math1/expt y 6))))))
  

(defn five-var-fn [a b c d e] (* (+ a (* b c)) (math1/expt (+ d e) 3)))
(def five-var-limits [[4 22] [0.1 0.8] [-1 1] [-10 5] [1 2]])
 
(defn around? [x val] (< (math1/abs (- x val)) 0.01))

(deftest montecarlo-multiscale
  (is true (let [[x y] (mc simple limits 3 10 1000 5)] (and (around? x 0) (around? y 0))))
  (is true (let [[x y] (mc complex limits 3 10 1000 5)] (and (around? x -1) (around? y -0.48))))
  (is true (let [[a b c d e] (mc five-var-fn five-var-limits 5 10 10000 5)] 
	(and (around? a 22) (around? b 0.64) (around? c 0.42) (around? d -10) (around? e 1))))
  )
	
