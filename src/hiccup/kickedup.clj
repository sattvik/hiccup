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
  [item & items]
    (if (and (string? item)   ;; If we can, merge the two strings.
	     (string? (last steps)))
      (set! steps (assoc steps
		    (dec (count steps))
		    (str (last steps) item)))
      (set! steps (conj steps item)))   ;; Otherwise just add whatever.
    (when (not (empty? items))
      (apply emit items)))

(declare literal?)
(defn- literal-map?
  "Tests whether a map is a literal (ie, all keys and values literal)."
  [m]
  (and (literal? (keys m))
       (literal? (vals m))))

(defn- literal-vector?
  "Tests whether a vector is a literal (all values literal)."
  [v]
  (every? #(or (literal? %)
	       (vector? %)
	       (and (map? %) (literal-map? %)))
	  v))

(defn- literal-list?
  "Tests whether a list is a literal (all values literal)."
  [l]
  (every? literal? l))

(defn literal?
  "Tests whether or not the argument is a literal we can compile (ie,
   it's either a simple literal, or a supported collection of literals)."
  [x]
  (cond (vector? x) (literal-vector? x)
	(map? x) (literal-map? x)
	(list? x) (literal-list? x)
	(seq? x) (literal-list? x)
	:otherwise (or (number? x) (string? x) (keyword? x) (nil? x)
		       (true? x) (false? x))))

(defn vector-has-form?
  "Takes two vectors; the first is compared element-wise against the second
   by applying each element of the second to each corresponding element of
   the first. Different sizes are not a match, although the keyword :any can
   be used to abort the test and return true if it is reached."
  [vtr form]
  (loop [curr-vtr vtr
	 curr-form form]
	(let [vtr-1 (first curr-vtr)
	      form-1 (first curr-form)]
	  ;; If they're both empty, or the current form is :any, return true.
	  (if (or (and (empty? curr-vtr) (empty? curr-form))
		  (= form-1 :any))
	    true ;; Same length and empty, so true.
	    (if (or (empty? curr-vtr) (empty? curr-form))
	      false ;; They're not both empty, but one and not the other is.
	      (if (or (nil? vtr-1) (nil? form-1)) ;; If either vector ran out...
		false
		;; Form is ok, apply test.
		(if (form-1 vtr-1)
		  (recur (rest curr-vtr) (rest curr-form))
		  false)))))))     ;; They didn't match.

(defn can-compile-vector?
  "Tests a vector to see if it's presented to us in a form that we can compile."
  [vtr]
  ;; Keep in mind, we test for all literals first, so only need to check for
  ;; cases that are not all literals.
  (cond (literal? vtr) true
	(vector-has-form? vtr [symbol?]) true
	(vector-has-form? vtr [symbol? literal? :any]) true
	(vector-has-form? vtr [literal? map? :any]) true
	(vector-has-form? vtr [symbol? map? :any]) true
	(vector-has-form? vtr [literal? symbol?]) true
	(vector-has-form? vtr [literal? list?]) true
	:otherwise false))

(defn- perf-warning
  "If *warn-on-interpret* is true, outputs a performance warning."
  [form]
  (if *warn-on-interpret*
    (do
      (println "------------------------------------------------------")
      (println "Kickup warning - Could not compile the following form:")
      (println (str form))
      (println "This form will be interpreted."))))

(defn- format-attr
  "Turn a name/value pair into an attribute stringp"
  [name value]
  (str " " (as-str name) "=\"" (hiccup/escape-html value) "\""))

(defn- make-attrs
  "Turn a map into a string of sorted HTML attributes."
  [attrs]
  (cond (literal? attrs)
	(apply str
	       (sort
		(for [[attr value] attrs]
		  (cond
		   (true? value) (format-attr attr attr)
		   (not value)   ""
		   :otherwise    (format-attr attr value)))))
	:otherwise `(#'make-attrs ~attrs))) ;; Make the attrs at runtime.

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

(defn- compile-tag-2-ambiguous
  "Compile an HTML tag with 2 elements, of which the second is not a literal,
   to the code to render it as a string of HTML. This function generates the
   code to make crucial parsing decisions at runtime for things we can't know
   at compile time. Due to the nature of HTML, this requires this code to
   handle the closing tag (or lack of). It generates the content of the tag
   starting right after the tag-name."
  [tag tag-attrs attrs-or-content standalone?]
  (let [rendered-tag-attrs (make-attrs tag-attrs)]
    ;; The problem is we don't know if the non-literal second element
    ;; is a map of attributes or the content of the tag.
    (emit `(cond
	    (nil? '~attrs-or-content)
	    ;; No unparsed attributes or content, but something in the attrs
	    ;; had to be evaluated at runtime, so we do it that way.
	    (str (#'hiccup/make-attrs ~tag-attrs)
		 (if (#'container-tags ~tag) ;; No content, so container tag
		   (str "><" ~tag ">")       ;; decision depends on tag.
		   " />"))
	    (map? ~@attrs-or-content)
	    ;; There are unparsed attributes, so do the merge and format.
	    (str (#'hiccup/make-attrs (merge ~tag-attrs ~@attrs-or-content))
		 (if ~standalone? " />" ">"))
	    ;; Otherwise, it is to be treated as content. Note this means a
	    ;; closing tag; if it has content, it needs a closing tag.
	    :otherwise (str ~rendered-tag-attrs
			    ">" (#'hiccup/render-html ~@attrs-or-content)
			    "</" ~tag ">")))))

(declare compile-html)
(defn- compile-tag
  "Compile an HTML tag represented as a vector to the code to render
   it as a string of HTML."
  [element]
  (if (can-compile-vector? element)
    (let [[tag attrs content] (parse-element element)]
      (if (and (= (count element) 2)
	       (not (literal? element)))
	;; So we can compile it, but it's not all literals, and only 2 elements:
	;; Special case to handle deferred 2nd element, ambiguous til runtime.
	(if (container-tags tag)
	  (do
	    (emit "<" tag)
	    (compile-tag-2-ambiguous tag attrs content false))
	  (do
	    (emit "<" tag)
	    (compile-tag-2-ambiguous tag attrs content true)))
	;; Else, we have a routine that will compile it otherwise.
	(if (or content (container-tags tag))
	  (do
	    (emit "<" tag (make-attrs attrs) ">")
	    (compile-html content)
	    (emit "</" tag ">"))
	  (do
	    (emit "<" tag (make-attrs attrs) " />")))))
    (do (perf-warning element)
	(emit `(#'hiccup/render-tag ~element)))))

(defn- compile-html
  "Compile a Clojure data structure to the code to render it
   as a string of HTML."
  [data]
  (cond
   (vector? data) (compile-tag data)
   (seq? data)    (if (or (vector? (first data))
			  (string? (first data))
			  (number? (first data))
			  (symbol? (first data)))
		    (apply str (map compile-html data))
		    (do (perf-warning data)
			(emit `(apply str (map #'hiccup/render-html
					       (vector ~@data))))))
   (symbol? data) (emit `(apply str ~data))
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