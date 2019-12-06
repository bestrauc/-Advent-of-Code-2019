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

(defn parse-opcode [instruction]
 "Parse opcode DE ithe ABCDE opcode format."
 (let [opcode (mod instruction 100)
       modes (mapv #(get-digit instruction %) (range 2 5))]
   (case opcode
     1 {:f + :in-args 2 :out-args 1 :modes modes}
     2 {:f * :in-args 2 :out-args 1 :modes modes}
     3 {:f #(Integer/parseInt (read-line)) :in-args 0 :out-args 1 :modes modes}
     4 {:f println :in-args 1 :out-args 0 :modes modes}
     99 :halt
   )))

(defn load-addr [mem mode ip]
  (case mode
    0 (nth mem (nth mem ip))
    1 (nth mem ip)))

(defn store-addr [mem mode ip value]
  (case mode
    0 (assoc mem (nth mem ip) value)
    1 (assoc mem ip value)))

(defn eval-program
  ([code] (eval-program code 0))
  ([code ip]
  (let [instruction (nth code ip)
        opcode (parse-opcode instruction)
        pmodes (:modes opcode)
        addr-loader #(load-addr code (nth pmodes %) (+ ip % 1))
        val-storer #(store-addr code (nth pmodes %1) (+ ip %1 1) %2)]
    (when (not= opcode :halt)
      (let [in-args (:in-args opcode)
            in-values (mapv addr-loader (range in-args))
            result (apply (:f opcode) in-values)
            out-args (:out-args opcode)
            total-args (+ in-args out-args)
            no-output? (zero? out-args)]
        (if no-output?
          (eval-program code (+ ip total-args 1))
          (eval-program (val-storer (- total-args 1) result) 
                        (+ ip total-args 1))))))))

(def test-program (vector 3 0 4 0 99))
(eval-program problem-input)
