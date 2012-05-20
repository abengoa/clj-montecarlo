(ns cljmontecarlo.multiscale
	"A multiscale Montecarlo method for minimizing multivariable functions."
	(use clojure.math.combinatorics)
	(require 
		[clojure.contrib.generic.functor :as func]))

;; ---------- Area functions

(defn subdivide 
	"Subdivides a set of limits, into n segments for each variable. The limits are a sequence of elements, one for each variable.
	Each element is a 2-item vector specifying the minimum and maximum values allowed for that variable. After the subdivision
	the new list will have (count l)^n elements."
	[l n] 
	(let [newvars (for [[v1 v2] l] (let [step (/ (- v2 v1) n)] (for [i (range n)] [(+ v1 (* i step)) (+ v1 (* (inc i) step))])))]
		(apply cartesian-product newvars)))

;; ---------- Point generation functions

(defn generate-random-point 
	"Generates a random point in the specified area."
	[l]
	(for [[v1 v2] l] (let [s (- v2 v1)] (+ (rand s) v1))))

(defn generate-area-points 
	"Generates n points in the specified area."
	[l n] 
	(for [i (range n)] (generate-random-point l)))

(defn summarize-area-points 
	"Calculates a summary of all the points found in an area. Currently the metric used is the min function."
	[p] (reduce min p))

;; ---------- Montecarlo-related functions: iteration

(defn area-step 
	"Generates the needed points for an area and summarizes them."
	[l n fitness-fn] 
	(summarize-area-points (map #(apply fitness-fn %) (generate-area-points l n))))

(defn calculate-num-points 
	"Calculates how many points should be generated for each area based on the current probabilities."
	[l-probs n] 
	(let [m (reduce + (map second l-probs))]
		(reduce into {} (map (fn [[l p]] {l (int (* n (/ p m)))}) l-probs))))

(defn step-all-areas 
	"Generates points and summaries for each area."
	[l-probs n fitness-fn] 
	(let [point-count (calculate-num-points l-probs n)]
		(reduce into {} (map (fn [l] {l (area-step l (point-count l) fitness-fn)})  (keys l-probs)))))

(defn generate-new-probs 
	"Recalculates the probabilities of the minimum of the function being on each area, by comparing the
	summarized values of each area."
	[l-summaries]
	(let [average (/ (reduce + (vals l-summaries)) (double (count (vals l-summaries))))
		  dists (func/fmap #(- average %) l-summaries)
		  m (reduce min (vals dists))
		  adjusted-dists (func/fmap #(inc (- % m)) dists)]
		adjusted-dists))
		
(defn mc-iteration 
	"Runs an iteration for a level of the Montecarlo simulation. Takes a series of current areas, generates random points
	on each area (the number on each area depends on the estimated probability of the minimum being on said area) and
	recalculates the probabilities of the minimum being on each area." 
	[l-probs point-count fitness-fn]
	(generate-new-probs (step-all-areas l-probs point-count fitness-fn)))

;; ---------- Montecarlo-related functions: levels
	
(defn mc-level
	"Runs a level of the Montecarlo simulation. It will run a series of iterations, each one assigning 
	 more points to the most probable search areas and removing them from the least probable. Parameters 
	 are the same than for the mc function."
	[fitness-fn lims subdivision-step point-count iteration-count]
	(loop [index iteration-count
			l-probs (reduce into {} (map (fn [l] {l 1.0}) (subdivide lims subdivision-step)))]
			(if (neg? index) l-probs
				(recur (dec index) (mc-iteration l-probs point-count fitness-fn)))))

;; ---------- Montecarlo-related functions: simulation

(defn mc 
	"Runs a multilevel Montecarlo simulation to find the minimum of a k-variable function.
		fitness-fn : a function that receives k parameters, that we are looking to minimize.
		lims : the bounds of the search for the minimum. A sequence of elements, one element per variable. 
			Each element is a 2-item vector that specifies the minimum and maximum values allowed for the input variable.
		subdivision-step : number of segments each variable search space will be split into on each multilevel transition.
		subdivision-limit : maximum number of multilevel transitions (number of times the winner area is subdivided into smaller areas).
		point-count : number of random points on each step of the algorithm.
		iteration-count : number of iterations performed before selecting the best area and subdividing it."
	[fitness-fn lims subdivision-step subdivision-limit point-count iteration-count]
	(loop [index subdivision-limit
		   current-area lims]
		(if (neg? index) (map (fn [[v1 v2]] (/ (+ v1 v2) 2)) current-area)
			(let [subdivisions (mc-level fitness-fn current-area subdivision-step point-count iteration-count)
				  max-val (reduce max (vals subdivisions))
				  best (first (first (filter (fn [[s v]] (= v max-val)) subdivisions)))]
				(recur (dec index) best)))))