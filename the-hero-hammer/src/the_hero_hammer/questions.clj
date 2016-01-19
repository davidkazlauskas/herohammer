(ns the-hero-hammer.questions)

(defn question [id shortname question & options]
   {:id id
    :shortname shortname
    :question question
    :options (into [] options)})
