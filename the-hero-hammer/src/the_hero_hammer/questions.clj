(ns the-hero-hammer.questions)

(defn question [id shortname question & options]
   {:id id
    :shortname shortname
    :question question
    :options (into [] options)})

(defn questions [& args]
  (into [] (map-indexed
     #(apply question %1 %2) args)))

(defmacro questions-m [& args]
  (apply questions args))
