(require '[clojure.string :as string])
(require '[clojure.set :as cljset])

(def problem-input (->> "inputs/input3.txt" 
                        slurp 
                        string/split-lines 
                        (map #(string/split % #","))))

(defn trace-direction [steps [start_x start_y] direction distance]
  "Return a set of points visted by going in direction for a distance."
  (let [direction-fn (case direction
                      "U" (fn [d] [start_x (+ start_y d)])
                      "R" (fn [d] [(+ start_x d) start_y])
                      "D" (fn [d] [start_x (- start_y d)])
                      "L" (fn [d] [(- start_x d) start_y]))]
    [(+ steps distance)
     (direction-fn distance) 
     (into #{} (for [d (range distance)] (direction-fn d)))]))

(defn trace-add [[steps pos lat-map trace-set] direction-code]
  "Add for use in reduce -> follow the directions and accumulate their points.
  (We also accumulate steps and latencies etc. on the side - very ugly overall."
  (let [direction (subs direction-code 0 1)
        distance (Integer/parseInt (subs direction-code 1))
        [next-steps next-pos new-trace] (trace-direction steps pos direction distance)
        updated-trace-set (cljset/union trace-set new-trace)
        updated-lat-map (into lat-map (for [k new-trace] [k (conj (lat-map k []) next-steps)]))]
    [next-steps next-pos updated-lat-map updated-trace-set]))
        

(defn record-path [path]
  (let [[steps pos lat-map trace-set] (reduce trace-add [0 [0 0] {} #{}] path)]
    [lat-map (disj trace-set [0 0])]))
  
