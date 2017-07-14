(ns clojure-noob.core
  (:gen-class))


(defn add [x y]
   (+ x y))


(defn subt [j k]
   (- j k))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println "Hello My name is Rupesh Chavan"))

(+ 1 2 3)

(if true
  "if statement"
  "else statement")

(if (= 3 (add 1 2))
  "if statemsdfent"
  "else stmt")

(if false
  "hello")
;=>nil


(if (= 24 (add 5 2))
  "true"
  "false")

(if false
  (do (println "hello")
      (println "Rupesh"))
  (do (println "else")
      (println "part")))

(when true
  ( "hello Mindseed")
  "abra")
;no else for when

(if (= 4 (add 2 3))
  (do (println "true"))
  (do (println "false")))

(nil? 0)

(if (= 3 3)
  "true"
  "false"
  )

(or false :hahah :yo :rupesh)

(and true true)

(def a 2)


(def severity :mild)
(def error-message "hello")
(if (= severity :mild)
  (def error-message (str error-message "rupesh"))
  (def error-message (str error-message "mindseed")))



(def name1 "hello")
(println "" name1)

(hash-map :a 1 :b 2)

(get {:a 0 :b 1} :a)

(def a {:first-name "charlie"
         :last-name "chavan"})

(get a :first-name)

(get a :last-name)

(get {:a 0 :b 1} :c)

(get {:a 0 :b {:c "ho hum"}} [:b :c])

(get-in {:a 0 :b {:c "ho hum"}} [:b :c])

({:name1 "hello dsdsd"}:name1)

({:a 1 :b 2 :c 3} :a)

(def a [3 2 1])
(get a 4)

(def a (vector "a" "b" "c"))

(conj [1 2 3] 4)

(nth '(1 2 3 4) 0)


(hash-set 1 2 1 2)
#{"htllro" 20 :icicle}

(contains? #{:a 3 :b 4} :c)

(conj '(1 2 3 4) 5 6)

(+ 1 2 3 4)
(nth [1 2 3 4] 2)

((or nil  +) 1 2 3)

(defn example
  "this is doc string"
  [name]
  (str "hello " name " from mindseed")
  )

(example "rupesh ")

(defn two-params [a b]
  (str "hello Rupesh " a b))

(two-params "soaosdasda asddks" "dasdadsad")


(defn x-chop
"doc-string")

(defn exam
  []
  (+ 1 304)
  30
  "joe")

(exam)

#(alength (to-array [1 2 3 4 5]))


(= (#(alength (to-array %)) '(1 2 3 3 1)) 5)

(= (reverse [1 2 3 4 5]) [5 4 3 2 1])




(= (conj '() [1 2 3 4 5]) [5 4 3 2 1])

(reduce conj '() [:a :b :c])

(reduce #(cons %2) [4 5 6])

(= (#( + %) [1 2 3])6)





(def a (re-matcher #"\d" ))

(= (reduce + [1 2 3]) 6)

filter odd?

(filter odd?)
(filter odd? #{1 2 3 4 5})

(=(filter odd? #{1 2 3 4 5}) '(1 3 5))


(defn is-odd? [n]
  (if (zero? n)
    (println "odd")))

(is-odd? 3)




(loop [x 3]
  (when (> x 1)
    (println x)
    (recur (- x 2))))

(fn fib [x]



(loop [iter 1  acc 0]
  (if (> iter 4 )
    (println acc)
    (recur (inc iter) (+ acc iter))))



  (loop [i 0 acc 1]
    (when (< i 3))
    (println i)
    (+ (recur (dec i)) (recur (- i 2))))




(loop [fib [1]]
        (if (>= (count fib) 3)
          fib
          (recur (inc fib)) ())))




(defn factorial [n]
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(defn fib [n]
  (fib-iter 1 0 n))


(defn fib [cnt]
    (if (= cnt 0)
      b
      (fib (+ a b) a (- cnt 1))))

(defn fib [n]
  (fib-iter 1 0 n))

(defn fib-iter [a b counter]
  (if (= counter 0)
      b
      (fib-iter (+ a b) a (- counter 1))))

(fn [c]
  (let [f* (fn f [acc x]
             (if (coll? x) (reduce f acc (seq x)) (conj acc x))
             )]
    (reduce f* [] c)
    ))

(defn my-flatten [xs]
  (lazy-seq
   (reduce (fn --internal-flatten [col v]
             (if (sequential? v)
               (reduce --internal-flatten col v)
               (conj col v)))
           []
           xs)))


(fn [n]
  (let [fib (fn fib* [a]
          (cons a (lazy-seq (fib* b (+ b a)))))]
    (take n (fib 1 1))))




(defn pad [string]
  (if (= (seq string)  (into '() (seq string)))
      true
      false))





(flatten (seq {name: "Hubbert" :age 23}))



(defn b[s]
  (reverse (reduce (fn my-flatten [acc item]
                     (if (coll? item)
                       (reduce my-flatten acc item)
                       (conj acc item)))
                   '()
                   s)))








(mapcat [[3 2 1 10] [6 5 4] [8 1 2]])


(defn f1
  [n]
  [(- n 1) n (+ n 1)])

(map f1 [1 2 3])


(mapcat (fn flat [])
   (if (coll? x)
    (mapcat flat x)
    [x]))
(defn flat [x]
  (if (coll? x)
    (mapcat flat x)
    [x]))

(filter #(Character/isUpperCase %))

(defn cap [s]
  (reduce str (filter #(Character/isUpperCase %) s)))

((dedupe %)s)


(defn duplicates
  [s]
  (reduce
    #(if-not (= (last %1) %2)
      (conj %1 %2) %1)
    [] s))


(defn del-consecutive-dups [col]
  (mapcat set (#(partition-by identity %1) col)))



(defn dup [s]
  (reduce #()))

(defn dup [arr]
  (reduce #(if-not
              (= (last %1) %2)
            (conj %1 %2) %1)
          [] arr))

(defn t [a b c]
  (str topic "hello mindseed"))

(map t ["Rupesh"])

(reduce + [1 2 3])

(map str ["a" "b"]["c" "d"])


(take 3 [1 2 3 4 5 6])



(take1 3 [1 2 3 4 5])


  (defn take-imp [a & args]
    (println  args)
    (loop [i 0]
      (if-not (= i a)
        (println "hello"))
      (recur (+ i 1))))

(take-imp 3 [ 1 [2 4] 3 4])

(take 3 [2 [3 4] 4 [2 4] 6])

(count [ 1 [2 4] 3 4])



(filter (fn [x]
          (count x)1)
        ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""])

(filter (fn [x]
  (= (count x) 1))
  ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""])

(my-take 3 [ 1 [2 4] 3 4])

(defn my-take [a arr]
  )

(defn my-map-impl  [func arr]
  (reduce #(conj %1 (func %2)) [] arr)
  )

(my-map-impl inc [1 2 3 4])

(reduce + 1 [2 3])


(my-map-impl inc [1 2 3 4])



;-----right


(defn my-take [a arr]
  (if (> a (count arr))
    arr
    (if-not (= 0 a)
      (conj (my-take (dec a) (rest arr)) (first arr)))))


(my-take 3 [1 2 3 4])

;----right



(defn sum-even-numbers [nums]
         (if-let [nums (seq (filter even? nums))]
           (reduce + nums)
           "No even numbers found."))

(sum-even-numbers [1 3 5 6 8])






;----right

(defn my-map-impl [f arr]
  (if (empty? arr)
    arr
    (conj (my-map-impl f (rest arr)) (f (first arr)))))


(my-map-impl inc [1 2 3 4])


;-----right






(reduce conj #{} [1 2 3 4 5])

(conj [1 2 3 5] 4)


(concat [1 2] [4 5])










(defn my-conj [arr] x
         (if
           (recur (my-conj arr x) (first xs) (next xs))(coll x)))

(conj [1 2 3] 4)








(defn my-map-impl [f arr]
  (if (empty? arr)
    arr
    (conj (my-map-impl f (rest arr)) (f (first arr)))))

(my-map-impl inc [1 2 3 4])


 (defn my-take [a arr]
    (if (> a (count arr))
      arr
      (if-not (= 0 a)
        (conj (my-take (dec a) (rest arr)) (first arr)))))


(defn my-drop [a arr]
  (if (< a 1)
    arr
   (conj (my-drop (dec a) (rest arr)) (first arr)) ))

(my-drop 2 [1 2 3 4])

;-------right

(defn my-sort [arr x]
  (if-let [y (peek arr)]
    (if (> y x)
      (conj (pop arr) x y)
      (conj arr x))
    [x]))

(defn bubble-sort [arr2]
  (let [arr3 (reduce bubble [] arr2)]
    (if (= arr2 arr3)
      arr2
      (recur arr3))))



(bubble-sort [34 2 7 1])

;----right



(defn my-drop [a arr]
  (if (= 0 a)
    arr
    (conj (my-drop (dec a) (rest arr)) (first arr))))

(my-drop 2 [1 2 3 4])


;------right


(defn my-concat [arr1 arr2]

    (if (seq arr1)
      (cons (first arr1)
            (my-concat (rest arr1) arr2))
      arr2))

(my-concat [1 2] [4 5])




;-----right



(defn my-func [arr]
  (helper arr []))

(defn helper [in out]
  (if (empty? in)
    out
    (helper (rest in) (conj out (* 2(first in))))))

(my-func [1 2 3 4 5])




(defn my-reduce [f arr]
  (helper f arr 0))


  (defn helper [f in out]
    (if (empty? in)
      out
      (helper f (rest in) (f (first in) out ))))

  (my-reduce + [1 2 3 4])


  (defn my-take [a arr]
    (helper a arr 0))

  (defn helper [a in out]
    (if (= 0 a)
      out
      (helper (dec a) (rest in) (conj (first in)))))

  (my-take 6 [1 2 3 4 5])



(defn my-drop [a arr]
  (helper a arr []))

  (defn helper [a in out]
  (if (= 0 a)
    out
    (helper (dec a) (rest in) (conj out (first in)))))

(my-drop 2 [1 2 3 4])

(drop 2 [1 2 3 4])


  (defn my-some [f arr]
    (helper f arr 0))

  (defn helper [f in out]
    (if (empty? in)
      out
      (helper f (rest in) (f (first in) out))))

  (my-some even? [1 2 3 4])

(defn my-filter [f arr]
  (helper f arr []))

(defn helper [f in out ]
  (if (empty? in)
    out
    (helper f (rest in)
            (if (= true (f (first in)))
              (conj out (first in))
              out))))

(my-filter even? [1 3 3 4 5 6 7 8 ])



(defn my-some? [x]
  (helper x 0))

(defn helper [x out]
  (if-not (nil? x)
    true
    false))

(my-some? nil)

(defn my-some [f arr]
  (helper f arr 0))

(defn helper [f in out]
  (if (empty? in)
    out
    (helper f (rest in) (f (first in) out))))


(my-some even? [1 2 3 4])

(some even? [1 2 3 4])


(defn my-sort [in-val]
  (process [] in-val))

(defn process [sorted-val val]
  (if (empty? val)
    sorted-val
    (let [min-val (apply min val)]
      (process (conj sorted-val min-val) (my-remove-first min-val val)))))


(defn my-remove-first [x arr]
  (if (empty? arr)
    '()
    (if (= (first arr) x)
      (rest arr)
      (cons (first arr) (my-remove-first x (rest arr))))))


(my-sort [23 5 78 1 12 12])

(remove #(= 4 %) [1 2 3 4])

(my-remove-first 3 [1 2 3 4 3 5])


(defn my-complement [f1]
  (fn [& a]
    (not (apply f1 a))))





(def not-empty? (my-complement empty?))

(not-empty? [1 2])



(defn my-into  [a arr]
  (apply conj a arr))

(my-into [2] [1 4 5])




(defn my-partial [f & a]
  (fn [& args]
    (apply f (my-into args a))))

(def sub (my-partial - 20))

(sub 3)




(defn my-sort-by [f args]
  )


(defn my-map [f arr]
  (into [] (map f arr))
  (my-min (apply map vector[arr (into [] (map f arr))]) []))

(my-map count ["aaaaa" "bbbb"])



(defn my-sort-impl [l]
  )



(defn my-min [in out]
  (if (empty? in)
    (my-sort-impl out)
    (if (< (second(first in)) (second(second in )))
      (my-min (rest in) (conj out (second(first in ))))
      (my-min (rest in) out))))

(defn my-remove
  )


(defn my-hash )


(defn h-map [arr marr hmap]
  (if (empty? marr)
    hmap
    (recur (rest arr) (rest marr)
           (conj hmap (vector (first arr) (first marr))))))

(defn f1 [f arr]
  (sortby (h-map arr (map f arr) []) [] ))

(defn min-by
  ([arr]
   (min-by (rest arr) (first arr)))
  ([arr min]
   (if (empty? arr)
     min
     (min-by (rest arr) (if (< (second (first arr)) (second min))
                          (first arr)
                          min)))))


(defn remove-by [arr min]
  (vec (concat (subvec arr 0 (.indexOf arr min)) (subvec arr (inc (.indexOf arr min))))))

(defn sortby [arr my-arr]
  (if (empty? arr)
    my-arr
    (sortby (remove-by arr (min-by arr)) (conj my-arr (first (min-by arr))))))

(f1 count ["aaa" "bb" "ccccc"])


(def a (h-map [1 2 3 4] ["aa" "bbb"] []))

(remove-by a [1 "aa"])

(rest a)
