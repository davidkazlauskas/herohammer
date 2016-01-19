(ns the-hero-hammer.questions-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.questions :refer :all]))


(deftest question-tests
  (testing "Generate expected"
  (is (= (question 0 "some question" "yes" "no")
         {:id 0 :shortname "some question"
          :question "yes" :options ["no"]}
         ))))
