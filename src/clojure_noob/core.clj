(ns clojure-noob.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn helper [in out]
  (if (empty? in)
    out
    (helper (rest in) (conj out (* 2(first in))))))

(defn my-func [arr]
  (helper arr []))

(defn helper1 [f in out]
  (if (empty? in)
    out
    (helper1 f (rest in) (f (first in) out ))))

(defn my-reduce [f arr]
  (helper1 f arr 0))



(defn my-take [a arr]
  (if (> a (count arr))
    arr
    (if-not (<= a 0)
      (conj (my-take (dec a) (rest arr)) (first arr)) ())))

(defn my-map-impl [f args]
  (if (empty? args)
    args
    (conj (my-map-impl f (rest args)) (f (first args)))))




(defn my-func [arr]
  (helper arr []))

(defn helper [in out]
  (if (empty? in)
    out
    (helper (rest in) (conj out (* 2(first in))))))


(defn my-filter [f arr]
  (helper f arr []))

(defn helper [f in out]
  (if (empty? in)
    out
    (helper f (rest in)
            (if (= true (f (first in)))
              (conj out (first in))
              out))))


(defn my-some? [x]
  (helper x 0))

(defn helper [x out]
  (if-not (nil? x)
    true
    false))



(defn my-complement [f1]
  (fn [& a]
    (not (apply f1 a))))



(def not-empty? (my-complement empty?))



(defn my-into  [a arr]
  (apply conj a arr))



(defn my-partial [f & a]
  (fn [& args]
    (apply f (my-into args a))))


(defn f
  ([n arr1]
   (f n 0 arr1 []))

  ([n y in out]
   (if (empty? in)
     out
     (if (zero? n)
       (f y n (rest in) out)
       (f (dec n) (inc y) in (conj out (first in)))))))


(defn odd-pos
  ([arr]
   (odd-pos true arr []))

  ([bool in out]
   (if (empty? in)
     out
     (if (= true bool)
       (odd-pos (not bool) (rest in) out)
       (odd-pos (not bool) (rest in) (conj out (first in)))))))



(defn sum
  ([arr]
   (sum arr 0))
  ([arr out]
   (if (empty? arr)
     out
     (sum (rest arr) (+ (first arr) out)))))



(require '[clojure.string :as str])
(defn xyz
  [text]
  (str/replace (str/trim text) #"lol" "LOL"))



(defn my-comp
  [f1 f2]
  (fn [& args]
    (f1 (apply f2 args))))

((my-comp inc *) 2 3)

(defn n-comp
  [& arr1]
  (fn [& arr2]
    (reduce #(%2 %1)
            (apply (last arr1) arr2)
            (rest (reverse arr1)))))




(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))


(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))


(require '[clojure.string :as s])
(defn clean
  [text]
  (reduce (fn [string string-fn] (string-fn string))
          text
          [s/trim #(s/replace % #"lol" "LOL")]))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))
(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))


(defn valid-moves
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))


(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))


(def my-board (assoc-in (new-board 5) [4 :pegged] false))




                                        ;problem 1 Multiples of 3 and 5

(defn mul
  [n]
  (reduce + (filter #(zero? (min (mod % 3) (mod % 5))) (range n))))

                                        ;even fibo numbers


(defn fibo
  ([]
   (fibo 1 1))
  ([a b]
   (lazy-seq (cons a (fibo b (+ a b))))))


(def a (reduce + (take-while #(<= % 4000000) (filter even? (fibo)))))


                                        ;Largest prime factor

#_(defn factor
  [n]
  (primefact []  (filter (fn [x] (if (= (mod n x) 0) x)) (range 1 n))))


#_(defn primefact
  [in out]
  (if (empty? out)
    in
    (recur (if (= true (prime? first out))
             (conj in (first out)))
           (rest out))))



#_(defn prime? [n]
  (= 2 (reduce +
               (for [i (range 1 (inc (n)))]
                 (if (= 0 (mod (n) i)) 1 0)))))

(defn prime-factors [n]
  (loop [n n
         div 2
         factors []]
    (if (< n 2)
      (last factors)
      (if (= 0 (rem n div))
        (recur (/ n div)
               div
               (conj factors div))
        (recur n
               (inc div)
               factors)))))


                                        ;Largest palindrome product

(defn palin? [s]
  (if (= (str s) (apply str (reverse (str s))))
    true
    false))
;remaining

                                        ; smallest multiple


(defn gcd
  [x y]
  (if (zero? y)
    x
    (recur y (mod x y))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(reduce lcm (range 1 20))


                                        ;sum square difference


(defn exp [x n]
  (if (zero? n) 1
      (* x (exp x (dec n)))))


(defn sum-of-square
  [n]
  (reduce + (map #(* % %) (range 1 (inc n)))))

(defn final
  [n]
  (- (exp (reduce + (range 1 (+ n 1))) 2) (sum-of-square n)))
