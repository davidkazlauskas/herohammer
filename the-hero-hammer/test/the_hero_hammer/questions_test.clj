(ns the-hero-hammer.questions-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.questions :refer :all]))


(defn testvector []
  '(("shorta" "some q a" "yes" "no") ("shortb" "some another" "1" "0")))

(defmacro testvector-m []
  (testvector))

(deftest question-tests
  (testing "Generate expected"
  (is (= (question 0 "sname" "some question" "yes" "no")
         {:id 0 :shortname "sname"
          :question "some question" :options ["yes" "no"]}
         ))
  (is (= (apply questions (testvector))
         [
          {:id 0 :shortname "shorta" :question "some q a"
           :options ["yes" "no"]}
          {:id 1 :shortname "shortb" :question "some another"
           :options ["1" "0"]}
         ]
         ))
  (is (= (get-question "shortb" (apply questions (testvector)))
         1))
  (is (= (get-question-and-answer-id "shortb" "0" (apply questions (testvector)))
         [1 1]))
  ))
