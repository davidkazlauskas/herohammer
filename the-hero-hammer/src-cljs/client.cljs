(ns the-hero-hammer.js_client
  (:require [ajax.core :as aj]))

(defn by-id [the-id]
  (.getElementById js/document the-id))

(defn sel-value [element]
  (aget
    (aget
      (aget element "options")
      (aget element "selectedIndex"))
    "value"))

(defn set-attrib [element attr the-val]
  (.setAttribute element attr the-val))

(defn construct-link-to-results
  [hu-s ho-s flt-s]
  (str js/matchupLink "/"
       hu-s "-"
       ho-s "-"
       flt-s))

(defn construct-link-to-10-random-comments []
  (str js/randCommentsLink
       "/" js/heroUser "-" js/heroOpponent))

(defn thumb-link [opt]
  (aget js/heroSquares opt))

; hero-user hero-opponent user-filter
(defn ^:export goToMatchup []
  (let [hu (by-id "hero-user")
        ho (by-id "hero-opponent")
        flt (by-id "user-filter")
        hu-s (sel-value hu)
        ho-s (sel-value ho)
        flt-s (sel-value flt)]
    (aset js/window "location"
          (construct-link-to-results hu-s ho-s flt-s))
    ))

(defn ^:export updateHeroSquares []
  (let [hu (by-id "hero-user")
        ho (by-id "hero-opponent")
        hu-s (sel-value hu)
        ho-s (sel-value ho)
        hu-l (thumb-link hu-s)
        ho-l (thumb-link ho-s)
        tu (by-id "thumb-user")
        to (by-id "thumb-opponent")]
    (set-attrib tu "src" hu-l)
    (set-attrib to "src" ho-l)))

(defn ^:export show10RandomComments []
  (let [to-get (construct-link-to-10-random-comments)]
    (aj/GET to-get :handler
            (fn [output] (js/alert output)))))
