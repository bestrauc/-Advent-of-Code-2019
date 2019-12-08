(require '[clojure.string :as string])

(defn add-orbit [graph orbit-string]
  "Add an orbit like 'B)C' to the {center1 [orbiters1..], ..} graph."
  (let [[center orbiter] (string/split orbit-string #"\)")
        existing-orbiters (or (graph center) [])]
    (assoc graph center (conj existing-orbiters orbiter))))

(defn build-graph [orbit-strings] 
  (reduce add-orbit {} orbit-strings))

(defn count-orbits 
  "Count orbits starting at COM."
  ([graph] (count-orbits graph 0 "COM"))
  ([graph orbit-sum center]
   (let [orbiters (graph center)
         orbiter-orbit-counts (mapv #(count-orbits graph (inc orbit-sum) %) orbiters)]
     (apply + orbit-sum orbiter-orbit-counts))))

(def test-input (string/split (slurp "test_input.txt") #"\n"))
(def test-orbit-graph (build-graph test-input))
(println "This should be 42:" (count-orbits test-orbit-graph))

(def problem-input (string/split (slurp "input6.txt") #"\n"))
(def orbit-graph (build-graph problem-input))
(println "The number of orbits is:" (count-orbits orbit-graph))
