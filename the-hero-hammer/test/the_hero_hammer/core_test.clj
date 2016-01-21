(ns the-hero-hammer.core-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(defn test-form []
  {"user-hero" 7 "opponent-hero" 11 "gank" 1 "last-hit" 0 "user-comment" "meow"})

(deftest form-test
  (testing "form processing test"
    (is (= (form-to-data (test-form))
           {:hero-user 7
            :hero-opponent 11
            :comment "meow"
            :answers [[8 1] [9 0]]}))
    ))
