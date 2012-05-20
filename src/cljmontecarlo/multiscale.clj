(ns cljmontecarlo.multiscale
	(use clojure.math.combinatorics)
	(require 
		[clojure.math.numeric-tower :as math1] 
		[clojure.contrib.generic.math-functions :as math2]
		[clojure.contrib.generic.functor :as func]))


;; x+y+x^2+y^2+3^sin(x^5+y^6)
(defn ff [x y] (+ x y (* x x) (* y y) (math1/expt 3 (math2/sin (+ (math1/expt x 5) (math1/expt y 6))))))
(defn simple [x y] (+ (* x x) (* y y)))


(def limits [[-1.0 1.0] [-1.0 1.0]])

(defn subdivide [l n] (let [newvars (for [[v1 v2] l] (let [step (/ (- v2 v1) n)] (for [i (range n)] [(+ v1 (* i step)) (+ v1 (* (inc i) step))])))]
	(apply cartesian-product newvars)))
	
(defn generate-random-point [l]  (for [[v1 v2] l] (let [s (- v2 v1)] (+ (rand s) v1))))

(defn generate-area-points [l n] 
(for [i (range n)] (generate-random-point l)))


;(defn summarize-area-points [p] (/ (reduce + p) (double (count p))))
(defn summarize-area-points [p] (reduce min p))


(defn area-step [l n fitness-fn] (summarize-area-points (map #(apply fitness-fn %) (generate-area-points l n))))

(defn calculate-num-points [l-probs n] 
	(let [m (reduce + (map second l-probs))]
		(reduce into {} (map (fn [[l p]] {l (int (* n (/ p m)))}) l-probs))))

(defn step-all-areas [l-probs n fitness-fn] 
	(let [point-count (calculate-num-points l-probs n)]
		(reduce into {} (map (fn [l] {l (area-step l (point-count l) fitness-fn)})  (keys l-probs)))))

(defn generate-new-probs [l-summaries]
	(let [average (/ (reduce + (vals l-summaries)) (double (count (vals l-summaries))))
		  dists (func/fmap #(- average %) l-summaries)
		  m (reduce min (vals dists))
		  adjusted-dists (func/fmap #(inc (- % m)) dists)]
		adjusted-dists))
		
(defn mt-iteration [l-probs point-count fitness-fn]
	(generate-new-probs (step-all-areas l-probs point-count fitness-fn)))
		
		

(defn mt-level [fitness-fn lims subdivision-step point-count iteration-count]
	(loop [index iteration-count
			l-probs (reduce into {} (map (fn [l] {l 1.0}) (subdivide lims subdivision-step)))]
			(if (neg? index) l-probs
				(recur (dec index) (mt-iteration l-probs point-count fitness-fn)))))

(defn mt [fitness-fn lims subdivision-step subdivision-limit point-count iteration-count]
	(loop [index subdivision-limit
		   current-area lims]
		(if (neg? index) (map (fn [[v1 v2]] (/ (+ v1 v2) 2)) current-area)
			(let [subdivisions (mt-level fitness-fn current-area subdivision-step point-count iteration-count)
				  max-val (reduce max (vals subdivisions))
				  best (first (first (filter (fn [[s v]] (= v max-val)) subdivisions)))]
				(recur (dec index) best)))))

;(mt ff limits 3 1 1 1000)