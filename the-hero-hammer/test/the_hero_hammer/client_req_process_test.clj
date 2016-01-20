(ns the-hero-hammer.client_req_process-test
  (:require [clojure.test :refer :all]
            [the-hero-hammer.client_req_process :refer :all]))

(deftest client-req-process-tests
  (testing "db key schema"
    (is (= (lol-gen-key-for-count "tryndamere" "akali" 0 1)
           "lol-question-count-104-2-0-1"))
    (is (= (lol-question-count-key) "lol-total-question-counter"))
    (is (= (lol-question-by-id-key 7) "lol-question-by-id-7"))
    (is (= (lol-gen-key-for-matchup-question-count "tryndamere" "akali")
           "lol-question-index-count-104-2"))
    (is (= (lol-question-by-matchup-and-id-key "tryndamere" "akali" 7)
           "lol-question-by-matchup-and-id-104-2-7"))
    ))

