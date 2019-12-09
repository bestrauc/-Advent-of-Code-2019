(require '[clojure.string :as string])
(require 'intcode-computer)

; Day 7 ---

(defn eval-with-params [mem phase signal]
  "Feed the phase/signal inputs to (read-line) and return the (println ..) output."
  (doto
    (with-out-str
      (with-in-str (format "%s\n%s" phase signal)
        (eval-program mem))) println))

(defn line-to-ints [line]
  (mapv #(Integer/parseInt %) (string/split line #",")))

(def input-mem (-> "inputs/input7.txt" slurp string/trim-newline line-to-ints))

; Part 1: Try out all combinations and find the one with the maximum output.
(println "Maximum without feedback:"
  (apply max-key :out
    (for [m1 (range 5)
          m2 (range 5)
          m3 (range 5)
          m4 (range 5)
          m5 (range 5)
          :when (== (count (hash-set m1 m2 m3 m4 m5)) 5)]
      (let [out1 (eval-with-params input-mem m1 0)
            out2 (eval-with-params input-mem m2 out1)
            out3 (eval-with-params input-mem m3 out2)
            out4 (eval-with-params input-mem m4 out3)
            out5 (eval-with-params input-mem m5 out4)]
        {:m1 m1 :m2 m2 :m3 m3 :m4 m4 :m5 m5 :out (Integer/parseInt (string/trim-newline out5))}))))

; TODO: The input str is exhausted at some point, causing issues.
; Part 2: Try out all combinations and find the one with the maximum output.
(println "Maximum without feedback:"
  (apply max-key :out
    (for [m1 (range 5 10)
          m2 (range 5 10)
          m3 (range 5 10)
          m4 (range 5 10)
          m5 (range 5 10)
          :when (== (count (hash-set m1 m2 m3 m4 m5)) 5)]
      (let [out1 (eval-with-params input-mem m1 0)
            _ (println out1)
            out2 (eval-with-params input-mem m2 out1)
            _ (println out1)
            out3 (eval-with-params input-mem m3 out2)
            _ (println out3)
            out4 (eval-with-params input-mem m4 out3)
            _ (println out4)
            out5 (eval-with-params input-mem m5 out4)
            _ (println out5)]
        {:m1 m1 :m2 m2 :m3 m3 :m4 m4 :m5 m5 :out 5}))))
