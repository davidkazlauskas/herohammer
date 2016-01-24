(ns the-hero-hammer.hh_process
  (:require [the-hero-hammer.hh_context :refer :all]))

(defn global-question-count []
  (get-key ((fn-global-question-count))))

(defn lol-process-pairs [to-process]
  (let [currmax (global-question-count)]
    (into [] (map
       (partial lol-process-single-pair currmax)
       to-process))))
