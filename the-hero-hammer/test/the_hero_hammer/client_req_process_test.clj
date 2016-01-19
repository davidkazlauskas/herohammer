(ns the-hero-hammer.client_req_process-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.client_req_process :refer :all]))

(deftest client-req-process-tests
  (testing "db key schema"
    (is (= (gen-key-for-count-lol "tryndamere" "akali" 0 1)
           "lol-question-count-tryndamere-akali-0-1"))
    (is (= (lol-question-count-key) "lol-total-question-counter"))
    (is (= (lol-question-by-id-key 7) "lol-question-by-id-7"))
    ))

