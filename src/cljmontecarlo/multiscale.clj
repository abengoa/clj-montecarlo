(ns cljmontecarlo.multiscale
	(use clojure.math.combinatorics)
	(require 
		[clojure.math.numeric-tower :as math1] 
		[clojure.contrib.generic.math-functions :as math2]))


;(defn grid

;(defn montecarlo-multiscale [partition-fn  fitness-fn projectile-amount subdivision-limit]




(defn ff [x y] (+ x y (* x x) (* y y) (math1/expt 3 (math2/sin (+ (math1/expt x 5) (math1/expt y 6))))))



(def limits [[-1.0 1.0] [-1.0 1.0]])

(defn subdivide [l n] (let [newvars (for [[v1 v2] l] (let [step (/ (- v2 v1) n)] (for [i (range n)] [(+ v1 (* i step)) (+ v1 (* (inc i) step))])))]
	(apply cartesian-product newvars)))
	
(defn generate-random-point [l]  (for [[v1 v2] l] (let [s (- v2 v1)] (+ (rand s) v1))))

(defn generate-area-points [l n] (for [i n] (generate-random-point l)))

(defn generate-points [l-probs n] (let [m (reduce + (map second l-probs))]
	(map (fn [[l n]] [l (generate-area-points l n)]) (map (fn [[l p]] [l (* n (/ p m))]) l-probs))))

(defn mt [fitness-fn lims subdivision-step subdivision-limit subdivision-threshold point-count]
	(let [l-probs (reduce into {} (map (fn [l] {l 1.0}) (subdivide lims subdivision-step)))]
		(let [totals (map (fn [[l [& points]]] {l (reduce (fn [[current-max current-p current-total] p] 
										(let [v (apply fitness-fn p)
											  [new-max new-p] (if (> v current-max) [v p] [current-max current-p])]
											   [new-max new-p (+ current-total v)]))
											   {l [Double/MIN_VALUE nil 0]} points)}) (generate-points l-probs point-count))
				best-element (reduce (fn [[cm cp] [l [m p _]]] (if (> m cm) [m p] [cm cp])) [Double/MIN_VALUE nil] totals)]
											   best-element)
	))
	
;(mt ff limits 3 1 1 1000)