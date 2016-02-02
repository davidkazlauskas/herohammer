(ns the-hero-hammer.shared_context
  (:require [co.paralleluniverse.pulsar.core :as pulsar]))

(defn process-questions [map-red-job]
  (let [max-units 128]
    (advance-map-reduce-job job max-units)))

(defn schedule-question-processing [map-red-job]
  (pulsar/spawn-fiber process-questions map-red-job))

(defn main-job [map-red-job]
  (while true
    (schedule-question-processing map-red-job)
    (pulsar/sleep (proc-delay-ms))))

(defn gen-jobs [map-red-job]
  (fn [] (pulsar/spawn-fiber main-job map-red-job)))

