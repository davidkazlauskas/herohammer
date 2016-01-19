(ns the-hero-hammer.questions_spec
  (:require [the-hero-hammer.questions :refer :all]))

(defmacro all-questions-lol []
  (questions-m
    ("mtype" "Match solo queue or ranked?"
           "Solo" "Ranked")
    ("ladder" "What is your ranking?"
              "Bronze" "Silver" "Gold" "Platinum"
              "Diamond" "Master" "Challenger")
    ))
