(ns the-hero-hammer.lol_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.storage :refer :all]
            [taoensso.nippy :as nippy]))

(def ^:dynamic *hh-context*
  {
   :dbinfo {
     :get-key the-hero-hammer.storage/get-key
     :set-key the-hero-hammer.storage/set-key
     :set-if-not-exists the-hero-hammer.storage/set-if-not-exists
   }
  })
