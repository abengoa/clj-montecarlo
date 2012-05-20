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
  
(deftest montecarlo-multiscale
  (is true (let [[x y] (mt simple limits 3 10 1000 5)] (and (< x 0.01) (> x -0.01) (< y 0.01) (> y -0.01))))
  (is true (let [[x y] (mt complex limits 3 10 1000 5)] (and (< x -0.99) (> x -1) (< y -0.47) (> y -0.49)))))
	