(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn read-lines [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (line-seq rdr))))

(defn line-to-ints
  [line]
  (mapv #(Integer/parseInt %) (str/split line #",")))

; The actual function that solves the problem.
(defn eval-memory
  ([input] (eval-memory input 0))
  ([input idx]
  (let [opcode (nth input idx)]
    (case opcode
      99 input
      (let [arg1 (nth input (nth input (+ idx 1)))
            arg2 (nth input (nth input (+ idx 2)))
            dest (nth input (+ idx 3))
            op (if (== opcode 1) + *)
            res (op arg1 arg2)
            next-state (assoc input dest res)]
        (eval-memory next-state (+ idx 4)))))))


; Check the example inputs to see if it works.
(def sample-inputs
  (mapv line-to-ints (read-lines "test_inputs.txt")))

(pprint/pprint (map vector sample-inputs (map eval-memory sample-inputs)))
(println)

; -----------------------------------------------------------------------------

; Actually solve the puzzle input
(def day2-input (line-to-ints (first (read-lines "input2.txt"))))

; Utility function to set the memory that we need in part 2.
(defn set-input [mem n v] (assoc (assoc mem 1 n) 2 v))

; Solve part 1.
(println (eval-memory (set-input day2-input 12 2)))
(println)

; Solve part 2 (brute-force).
(def solution-args 
  (first
      (for [n (range 100)
            v (range 100)
            :let [mem (set-input day2-input n v)]
            :when (== (first (eval-memory mem)) 19690720)]
        (vector n v))))

(let [[n v] solution-args]
  (printf "100*%s + %s = %s\n" n v (+ (* n 100) v)))
