(ns #^{:doc "Defines a couple of utility macros for testing hiccup’s various
            output modes"
       :author "Daniel Solano Gómez"}
  test.hiccup.test-utils
  (:use clojure.test
        hiccup.core))

(defmacro test-multi-output
  "Tests that the various output modes produce the appropriate output with a
  givn input."
  [expected & args]
  `(are [expected# actual#] (= expected# actual#)
        ~(expected :xhtml) (html        ~@args)
        ~(expected :html4) (html :html4 ~@args)
        ~(expected :xhtml) (html :xhtml ~@args)))

(defmacro test-same-multi-output
  "Tests that the various output modes produce the same output with a given
  input"
  [expected & args]
  `(test-multi-output {:xhtml ~expected
                       :html4 ~expected}
                      ~@args))
