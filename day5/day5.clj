(require '[clojure.string :as str])

(defn line-to-ints [line]
  (mapv #(Integer/parseInt %) (str/split line #",")))

; `->` and `slurp` borrowed from Daniel.
(def problem-input (-> "input5.txt" slurp str/trim-newline line-to-ints))
(println problem-input)

(defn get-digit [n i] 
  "Get the i-th digit of number n. 
  (The least significant digit is digit 0.)"
  (let [shift (->> i (Math/pow 10) int)]
    (mod (quot n shift) 10)))

(defn parse-instruction [mem]
 (let [instruction (first mem)
       opcode (mod instruction 100)
       pmodes (mapv #(get-digit instruction %) (range 2 5))]
   (case opcode
     1 {:f + :in-args 2 :out-pos 2 :pmodes pmodes}
     2 {:f * :in-args 2 :out-pos 2 :pmodes pmodes}
     3 {:f #(Integer/parseInt (read-line)) :in-args 0 :out-pos 0 :pmodes pmodes}
     4 {:f #(println "RESULT:" %) :in-args 1 :out-pos nil :pmodes pmodes}
     99 :halt
   )))

(defn ip-offset [opcode] 
  "Determine how far to advance the instruction pointer after this opcode.
  - for instructions with output, go to the pos after the output.
  - for instructions without output, go to the pos after the last input."
  (if-let [out-pos (:out-pos opcode)] (+ out-pos 2) (inc (:in-args opcode))))

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
            next-ip (+ ip (ip-offset opcode))]
        (if (some? (:out-pos opcode))
          (eval-program (assoc mem (load-addr (:out-pos opcode)) result) next-ip)
          (eval-program mem next-ip)))))))

(def test-program (vector 3 0 4 0 99))
(eval-program test-program)
;(eval-program problem-input)
