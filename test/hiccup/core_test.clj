(ns test.hiccup.core-test
  (:use clojure.test)
  (:use hiccup.core)
  (:use test.hiccup.test-utils))

(deftest tag-text
  (test-same-multi-output "<text>Lorem Ipsum</text>"
                          [:text "Lorem Ipsum"]))

(deftest empty-tags
  (test-multi-output {:xhtml "<text />"
                      :html4 "<text >"}
                     [:text]))

(deftest empty-block-tags
  (test-same-multi-output "<div></div>"
                          [:div])
  (test-same-multi-output "<h1></h1>"
                          [:h1])
  (test-same-multi-output "<script></script>"
                          [:script]))

(deftest empty-links-tag
  (test-same-multi-output "<a></a>"
                          [:a]))

(deftest tags-can-be-strs
  (test-same-multi-output "<div></div>"
                          ["div"]))

(deftest tags-can-be-symbols
  (test-same-multi-output "<div></div>"
                          ['div]))

(deftest tag-concatenation
  (test-same-multi-output "<body>foobar</body>"
                          [:body "foo" "bar"])
  (test-multi-output {:xhtml "<body><p /><br /></body>"
                      :html4 "<body><p ><br ></body>"}
                     [:body [:p] [:br]]))

(deftest tag-seq-expand
  (test-same-multi-output "<body>foobar</body>"
                          [:body (list "foo" "bar")]))

(deftest html-seq-expand
  (test-same-multi-output "<p>a</p><p>b</p>"
                          (list [:p "a"] [:p "b"])))

(deftest nested-tags
  (test-multi-output {:xhtml "<div><p /></div>"
                      :html4 "<div><p ></div>"}
                     [:div [:p]])
  (test-same-multi-output "<div><b></b></div>"
                          [:div [:b]])
  (test-same-multi-output "<p><span><a>foo</a></span></p>"
                          [:p [:span [:a "foo"]]]))

(deftest attribute-maps
  (test-multi-output {:xhtml "<xml a=\"1\" b=\"2\" />"
                      :html4 "<xml a=\"1\" b=\"2\" >"}
                     [:xml {:a "1", :b "2"}]))

(deftest blank-attribute-map
  (test-multi-output {:xhtml "<xml />"
                      :html4 "<xml >"}
                     [:xml {}]))

(deftest escaped-chars
  (is (= (escape-html "\"") "&quot;"))
  (is (= (escape-html "<") "&lt;"))
  (is (= (escape-html ">") "&gt;"))
  (is (= (escape-html "&") "&amp;"))
  (is (= (escape-html "foo") "foo")))

(deftest escaped-attrs
  (test-same-multi-output "<div id=\"&quot;\"></div>"
                          [:div {:id "\""}]))

(deftest attrs-can-be-strs
  (test-multi-output {:xhtml "<img id=\"foo\" />"
                      :html4 "<img id=\"foo\" >"}
                     [:img {"id" "foo"}]))

(deftest attrs-can-be-symbols
  (test-multi-output {:xhtml "<img id=\"foo\" />"
                      :html4 "<img id=\"foo\" >"}
                     [:img {'id "foo"}]))

(deftest attr-keys-different-types
  (test-multi-output {:xhtml "<xml a=\"1\" b=\"2\" c=\"3\" />"
                      :html4 "<xml a=\"1\" b=\"2\" c=\"3\" >"}
                     [:xml {:a "1", 'b "2", "c" "3"}]))

(deftest tag-class-sugar
  (test-same-multi-output "<div class=\"foo\"></div>"
                          [:div.foo])
  (test-same-multi-output "<div class=\"foo\">barbaz</div>"
                          [:div.foo (str "bar" "baz")]))

(deftest tag-many-class-sugar
  (test-same-multi-output "<div class=\"a b\"></div>"
                          [:div.a.b])
  (test-same-multi-output "<div class=\"a b c\"></div>"
                          [:div.a.b.c]))

(deftest tag-id-sugar
  (test-same-multi-output "<div id=\"foo\"></div>"
                          [:div#foo]))

(deftest tag-id-and-classes
  (test-same-multi-output "<div class=\"bar baz\" id=\"foo\"></div>"
                          [:div#foo.bar.baz]))

(deftest attrs-bool-true
  (test-multi-output {:xhtml "<input checked=\"checked\" type=\"checkbox\" />"
                      :html4 "<input checked=\"checked\" type=\"checkbox\" >"}
                     [:input {:type "checkbox" :checked true}]))

(deftest attrs-bool-false
  (test-multi-output {:xhtml "<input type=\"checkbox\" />"
                      :html4 "<input type=\"checkbox\" >"}
                     [:input {:type "checkbox" :checked false}]))

(deftest attrs-nil
  (test-same-multi-output "<span>foo</span>"
                          [:span {:class nil} "foo"]))

(deftest attrs-are-evaluated
  (test-multi-output {:xhtml "<img src=\"/foo/bar\" />"
                      :html4 "<img src=\"/foo/bar\" >"}
                     [:img {:src (str "/foo" "/bar")}])
  (test-same-multi-output "<div id=\"ab\">foo</div>"
                          [:div {:id (str "a" "b")} (str "foo")]))

(deftest content-can-be-vars
  (let [x "foo"]
    (test-same-multi-output "<span>foo</span>"
                            [:span x])))

(deftest optimized-forms
  (test-same-multi-output "<ul><li>0</li><li>1</li><li>2</li></ul>"
                          [:ul (for [n (range 3)]
                                 [:li n])])
  (test-same-multi-output "<div><span>foo</span></div>"
                          [:div (if true
                                  [:span "foo"]
                                  [:span "bar"])]))

(deftest type-hints
  (let [string "x", number 1]
    (test-same-multi-output "<span>x</span>"
                            [:span #^String string])
    (test-same-multi-output "<span>1</span>"
                            [:span #^Integer number])))
