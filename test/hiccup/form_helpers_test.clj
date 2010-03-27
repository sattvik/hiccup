(ns test.hiccup.form-helpers-test
  (:use clojure.test)
  (:use hiccup.core)
  (:use hiccup.form-helpers
        test.hiccup.test-utils))

(deftest test-hidden-field
  (test-multi-output {:xhtml "<input id=\"foo\" name=\"foo\" type=\"hidden\" value=\"bar\" />"
                      :html4 "<input id=\"foo\" name=\"foo\" type=\"hidden\" value=\"bar\" >"}
                     (hidden-field :foo "bar")))

(deftest test-text-field
  (test-multi-output {:xhtml "<input id=\"foo\" name=\"foo\" type=\"text\" />"
                      :html4 "<input id=\"foo\" name=\"foo\" type=\"text\" >"}
                     (text-field :foo)))

(deftest test-check-box
  (test-multi-output {:xhtml (str "<input checked=\"checked\" id=\"foo\" name=\"foo\""
                                  " type=\"checkbox\" value=\"true\" />")
                      :html4 (str "<input checked=\"checked\" id=\"foo\" name=\"foo\""
                                  " type=\"checkbox\" value=\"true\" >")}
                     (check-box :foo true)))

(deftest test-select-options
  (are [x y] (= (html x) y)
    (select-options ["foo" "bar" "baz"])
      "<option>foo</option><option>bar</option><option>baz</option>"
    (select-options ["foo" "bar"] "bar")
      "<option>foo</option><option selected=\"selected\">bar</option>"
    (select-options [["Foo" 1] ["Bar" 2]])
      "<option value=\"1\">Foo</option><option value=\"2\">Bar</option>"))
