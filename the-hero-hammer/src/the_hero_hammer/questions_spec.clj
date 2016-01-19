(ns the-hero-hammer.questions_spec
  (:require [the-hero-hammer.questions :refer :all]))

(defmacro all-questions-lol []
  (questions-m
    ("mtype" "Match solo queue or ranked?"
           "Solo" "Ranked")
    ("ladder" "What is your ranking?"
              "Bronze" "Silver" "Gold" "Platinum"
              "Diamond" "Master" "Challenger")
    ("poking" "I got poked a lot" "yes" "no")
    ("poking-o" "I poked my opponent a lot" "yes" "no")
    ("sustain" "I could not sustain well" "yes" "no")
    ("sustain-o" "My opponent could not sustain me" "yes" "no")
    ("first-blood" "I got first blood" "yes" "no")
    ("cc" "I got a lot of cc (stuns/slows)" "yes" "no")
    ("gank" "I was ganked quite often" "yes" "no")
    ("last-hit" "I had trouble last hitting minions" "yes" "no")
    ))
