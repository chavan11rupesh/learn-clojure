(ns clojure-noob.core-test
  (:require #_[clojure.test :refer :all]
            [clojure-noob.core :refer :all]
            [expectations :as expect]))

#_(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


#_(expect/expect empty? [])
(expect/expect empty? [])

;my-take tests



(expect/expect '(1) (my-take 1 [1]))


#_(expect/expect () (my-take 1 nil))

#_(expect/expect '(1 2 3) (my-take 3 [1 2 3 4]))

(expect/expect '(1 2 3) (my-take 3 [1 2 3 4]))


;my-map tests

(expect/expect '(2 3 4 5) (my-map-impl inc [1 2 3 4]))

(expect/expect '(1 2 3) (my-take 3 [1 2 3 4]))

(expect/expect '(2 3 4 5) (my-map-impl inc [1 2 3 4]))

(expect/expect '(0 1 2 2) (my-map-impl dec [1 2 3 3]))

;my-func tests

#_(expect/expect '(2 4 6 8) (my-func [1 2 3 4]))

;my-filter tests

#_(expect/expect [0 2 4 6 8] (my-filter even? (range 10)))

(expect/expect false (my-some? nil))


;my-sort tests
#_(expect/expect (3 6 7 12 34) (my-sort [34 12 6 7 3]))


(expect/expect true (not-empty? [1 2]))

;my-into tests

(expect/expect {:a 1, :b 2, :c 3} (my-into (sorted-map) [[:a 1] [:c 3] [:b 2]]))

(expect/expect [[1 2] [3 4]] (my-into [] {1 2, 3 4}))

(expect/expect [1 2 3 4 5 6] (my-into [1 2 3] '(4 5 6)))


(expect/expect 10 (sum [1 2 3 4]))

(expect/expect 10 (sum [1 2 3 4]))

(expect/expect "This is my text, LOL" (xyz "This is my text, lol"))


                                        ;test for project euler problems

(expect/expect 23 (mul 10))

(expect/expect 0 (mul 1))



(expect/expect 4613732 a)

(expect/expect 29 (prime-factors 13195))

(expect/expect 6857 (prime-factors 600851475143))

(expect/expect 173 (prime-factors (* 113 173)))

(expect/expect 2 (gcd 4 6))

(expect/expect 2520 (reduce lcm (range 1 10)))

(expect/expect 2640 (final 10))

(expect/expect 25164150 (final 100))
