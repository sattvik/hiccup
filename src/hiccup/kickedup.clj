(ns hiccup.kickedup
    "Compiles Hiccup syntax data structures to a fast function producing HTML."
    (:use clojure.contrib.def)
    (:use clojure.contrib.java-utils)
    (:use hiccup))

(defvar *warn-on-interpret* true
  "Set this binding to true to be warned when code fails to compile and
   is instead deferred to being interpreted at runtime.")

(defvar- steps []
  "This is a dynamically bound vector that builds up a list of strings and
   actions, which will then be compiled into operations on a StringBuilder.")
(defvar- builder-name 'string-builder
  "This will be the name of a lexically scoped variable that builds up
   the result string.")

(defn- emit
  "Append this item to the list of steps to compile. This is also where the
   logic of string optimization happens: if you emit two strings in a row,
   they get combined into one string."
  [item]
  (if (and (string? item)
	   (string? (last steps)))
    (set! steps (assoc steps
		  (dec (count steps))
		  (str (last steps) item)))
    (set! steps (conj steps item))))

(defn- can-compile-tag?
  "Returns true if the given tag can be compiled."
  [form]
  (let [fst (first form)
	snd (second form)]
    (and (or (keyword? fst)
	     (string? fst)
	     (symbol? fst))
	 (or (nil? snd)
	     (vector? snd)
	     (map? snd)
	     (number? snd)))))

(defn- perf-warning
  "If *warn-on-interpret* is true, outputs a performance warning."
  [form]
  (if *warn-on-interpret*
    (do
      (println "Kickup warning - Could not compile the following form:")
      (println (str form))
      (println "Falling back to interpreted mode."))))

(defn- format-attr
  "Turn a name/value pair into an attribute stringp"
  [name value]
  (str " " (as-str name) "=\"" (escape-html value) "\""))

(defn- make-attrs
  "Turn a map into a string of sorted HTML attributes."
  [attrs]
  (apply str
    (sort
      (for [[attr value] attrs]
	(cond
	 (true? value) (format-attr attr attr)
	 (not value)   ""
	 :otherwise    (format-attr attr value))))))

(defvar- re-tag
  #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?"
  "Regular expression that parses a CSS-style id and class from a tag name.")

(defvar- container-tags
  #{"a" "b" "body" "dd" "div" "dl" "dt" "em" "fieldset" "form" "h1" "h2" "h3"
    "h4" "h5" "h6" "head" "html" "i" "label" "li" "ol" "pre" "script" "span"
    "strong" "style" "textarea" "ul"}
  "A list of tags that need an explicit ending tag when rendered.")

(defn- parse-tag-name
  "Parse the id and classes from a tag name."
  [tag]
    (rest (re-matches re-tag (as-str tag))))

(defn- parse-element
  "Ensure a tag vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (let [[tag id class] (parse-tag-name tag)
	tag-attrs      {:id id
			:class (if class (.replace class "." " "))}
	map-attrs      (first content)]
    (if (map? map-attrs)
      [tag (merge tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

; This is an unmodified copy of hiccup/render-tag, only made
; non-private so we can make it get run at runtime.
(defn render-tag
  "Render a HTML tag represented as a vector. This is meant
   to be called at runtime. This function is just a slight
   modification of hiccup/render-tag."
  [element]
  (let [[tag attrs content] (parse-element element)]
    (if (or content (container-tags tag))
      (str "<" tag (make-attrs attrs) ">"
	   (hiccup/html content)
	   "</" tag ">")
      (str "<" tag (make-attrs attrs) " />"))))

(declare compile-html)
(defn- compile-tag
  "Compile an HTML tag represented as a vector to the code to render
   it as a string of HTML."
  [element]
  (if (can-compile-tag? element)
    (let [[tag attrs content] (parse-element element)]
      (if (or content (container-tags tag))
	(do
	  (emit "<")
	  (emit tag)
	  (emit (make-attrs attrs))
	  (emit ">")
	  (compile-html content)
	  (emit "</")
	  (emit tag)
	  (emit ">"))
	(do
	  (emit "<")
	  (emit tag)
	  (emit (make-attrs attrs))
	  (emit " />"))))
    (do (println (perf-warning element))
	(emit `(render-tag ~element)))))

(defn- compile-html
  "Compile a Clojure data structure to the code to render it
   as a string of HTML."
  [data]
  (cond
   (vector? data) (compile-tag data)
   (seq? data)    (if (or (vector? (first data))
			  (string? (first data))
			  (number? (first data)))
		    (apply str (map compile-html data))
		    (do (perf-warning data)
			(emit `(apply str (map hiccup/html ~@data)))))
   :otherwise     (emit (as-str data))))

(defmacro kickup
  "Compile Clojure data structs to fast functions producing a string of HTML."
  [[html & html-content]]
  (binding [steps []]
    (compile-html html-content)
    (let [the-steps (map #(list `.append `~builder-name %) steps)]
      ;; Squeeze steps down into a compiled function to return.
      `(let [~builder-name (StringBuilder.)]
	 ~@the-steps
	 (.toString ~builder-name)))))