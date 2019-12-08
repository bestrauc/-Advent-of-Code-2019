(require '[clojure.string :as string])

; TODO: Same intcode computer as day 5.
;       Copy it into a separate file.

(defn get-digit [n i] 
  "Get the i-th digit of number n. 
  (The least significant digit is digit 0.)"
  (let [shift (->> i (Math/pow 10) int)]
    (mod (quot n shift) 10)))

(defn parse-instruction [mem]
  "Parse the ABCDE instruction at the start of this mem region.
  
  ABC contain the parameter modes for parameters 3/2/1 and DE
  stores the actual opcode. We return the function that this
  opcode computes and data about its arguments and param modes."
 (let [instruction (first mem)
       opcode (mod instruction 100)
       pmodes (mapv #(get-digit instruction %) (range 2 5))]
   (case opcode
     1 {:f + :in-args 2 :out-pos 2 :pmodes pmodes}
     2 {:f * :in-args 2 :out-pos 2 :pmodes pmodes}
     3 {:f #(Integer/parseInt (read-line)) :in-args 0 :out-pos 0 :pmodes pmodes}
     4 {:f #(println %) :in-args 1 :out-pos nil :pmodes pmodes}
     5 {:f #(if (zero? %1) nil %2) :in-args 2 :out-pos nil :pmodes pmodes}
     6 {:f #(if (zero? %1) %2 nil) :in-args 2 :out-pos nil :pmodes pmodes}
     7 {:f #(if (< %1 %2) 1 0) :in-args 2 :out-pos 2 :pmodes pmodes}
     8 {:f #(if (= %1 %2) 1 0) :in-args 2 :out-pos 2 :pmodes pmodes}
     99 :halt
   )))

(defn get-next-ip-addr [ip opcode result] 
  "Get the next IP after the opcode has run and returned the result.
  - for instructions with output, simply go to the pos after the output.
  - for instructions without output (print or jumps), there are two options:
      - if they return `nil`, the IP just goes past their params
      - if they return a value, it's interpreted as an address and the IP set to it"
  (if-let [out-pos (:out-pos opcode)] 
    (+ ip out-pos 2) 
    (if-let [target-addr result] 
      target-addr
      (+ ip (inc (:in-args opcode))))))

(defn addr-getter [mem ip pmodes]
  "Return a function that loads the address at ip+offset. Depending
  on the parameter mode, it returns mem[ip+offset+1] or ip+offset+1."
  (fn [offset] 
    (let [offset-mode (nth pmodes offset)
          mem-offset (+ ip offset 1)]
    (case offset-mode
      0 (nth mem mem-offset)
      1 mem-offset))))

(defn eval-program
  ([mem] (eval-program mem 0))
  ([mem ip]
  (let [opcode (parse-instruction (subvec mem ip))
        load-addr (addr-getter mem ip (:pmodes opcode))]
    (when (not= opcode :halt)
      (let [in-addrs (mapv load-addr (range (:in-args opcode)))
            in-vals (mapv #(nth mem %) in-addrs)
            result (apply (:f opcode) in-vals)
            next-ip (get-next-ip-addr ip opcode result)]
        (if (some? (:out-pos opcode))
          (eval-program (assoc mem (load-addr (:out-pos opcode)) result) next-ip)
          (eval-program mem next-ip)))))))

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
