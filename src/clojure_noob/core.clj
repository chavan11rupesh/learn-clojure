(ns clojure-noob.core
  (:gen-class))

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
