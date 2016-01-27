(ns the-hero-hammer.js_client)

(defn by-id [the-id]
  (.getElementById js/document the-id))

(defn sel-value [element]
  (aget
    (aget
      (aget element "options")
      (aget element "selectedIndex"))
    "value"))

(defn construct-link-to-results
  [hu-s ho-s flt-s]
  (str js/matchupLink "/"
       hu-s "-"
       ho-s "-"
       flt-s))

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
