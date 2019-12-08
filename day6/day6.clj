(require '[clojure.string :as string])

; Model the orbits as an orbiter->center map,
; since that seems most useful for part 1 and 2.
(defn add-orbit [graph orbit-string]
  "Add an orbit like 'B)C' to the orbit->center graph."
  (let [[center orbiter] (string/split orbit-string #"\)")]
    (assoc graph orbiter center)))

(defn build-graph [orbit-strings] 
  (reduce add-orbit {} orbit-strings))

(defn count-orbits 
  "Count orbits by following all nodes to a sink and counting the steps.

  This straightforward, but a bit inefficient since it visits O(|E|^2) 
  edges even though O(|E|) would be sufficient with a bit more care."
  ([graph] (count-orbits graph (keys graph))) ; Initally all planets could orbit something.
  ([graph orbiters]
   ; Map all orbiters to its centers, filter those who don't orbit something.
   ; Then count the number of orbits and repeat for the found centers.
   (let [orbit-centers (filter some? (map graph orbiters))]
     (if (empty? orbit-centers) 
       0
       (+ (count orbit-centers) (count-orbits graph orbit-centers))))))

(defn count-transfers [graph source target] 
  "The number of transfers is the sum of the number of steps 
  to the LCA of the source and target nodes in the graph."
  (let [source-to-sink (take-while some? (iterate graph source))
        target-to-sink (take-while some? (iterate graph target))
        source-dists-to-sink (apply hash-map (interleave source-to-sink (range)))
        target-dists-to-sink (apply hash-map (interleave target-to-sink (range)))
        ; LCA is the 1st elem on the path to the sink that occurs in both paths.
        lca (first (filter #(contains? source-dists-to-sink %) target-to-sink))]
    ; Return the sum of the dists to LCA, -2 to account for the own orbits.
    (+ (target-dists-to-sink lca) (source-dists-to-sink lca) -2)))

; Tests and solving the inputs for part 1 & 2 ---------------------------------

(def test-input (string/split (slurp "test_input.txt") #"\n"))
(def test-orbit-graph (build-graph test-input))
(println test-orbit-graph)
(println "This should be 42:" (count-orbits test-orbit-graph))

(def problem-input (string/split (slurp "input6.txt") #"\n"))
(def orbit-graph (build-graph problem-input))
(println "The number of orbits is:" (count-orbits orbit-graph))

(def test-input2 (string/split (slurp "test_input2.txt") #"\n"))
(def test-orbit-graph2 (build-graph test-input2))
(println "YOU to SAN example should be 4:" (count-transfers test-orbit-graph2 "YOU" "SAN"))

(println "Distance in the problem: " (count-transfers orbit-graph "YOU" "SAN"))
