(ns the-hero-hammer.questions-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.questions :refer :all]))


(deftest question-tests
  (testing "Generate expected"
  (is (= (question 0 "sname" "some question" "yes" "no")
         {:id 0 :shortname "sname"
          :question "some question" :options ["yes" "no"]}
         ))
  (is (= (questions-m ("shorta" "some q a" "yes" "no")
                     ("shortb" "some another" "1" "0"))
         [
          {:id 0 :shortname "shorta" :question "some q a"
           :options ["yes" "no"]}
          {:id 1 :shortname "shortb" :question "some another"
           :options ["1" "0"]}
         ]
         ))
  ))
