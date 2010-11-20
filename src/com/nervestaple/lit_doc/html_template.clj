;;
;; These functions are used to generate the templated HTML code for
;; the documentation pages. They have been releaated to this file as
;; they are ugly and unpleasant.
;;
(ns com.nervestaple.lit-doc.html-template)

;;
;; This function renders the heading for the documentation page.
;;
(defn render-heading
  []
  (println "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
  (println "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"
lang=\"en\">")
  (println "<head>")
  (println "<title>Literate Documentation</title>")
; (println
; "<link href=\"http://www.nervestaple.com/google-code-prettify/src/prettify.css\"
; type=\"text/css\" rel=\"stylesheet\"/>
; <script type=\"text/javascript\"
; src=\"http://www.nervestaple.com/google-code-prettify/src/prettify.js\">
; </script><script type=\"text/javascript\"
; src=\"http://www.nervestaple.com/google-code-prettify/src/lang-lisp.js\">
; </script>")
    (println "<style type=\"text/css\">
body {margin: 1in; font-family: Georgia, Cambria, \"Times New Roman\", Times,
serif;  width: 6.5in; font-size: 12pt;}
pre, code, .code {font-family: Menlo, \"Lucida Console\", Monaco, monospace;
font-size: 9pt;}
.code {width: 6.35in; margin-left: 0.15in;}
pre.prettyprint {border: none; border-left: 3px solid LightGray;
margin-top: 0; margin-bottom: 0; border-collapse: collapse;
padding-left: 0.10in;}
div.inline-comment{margin-top: 0; margin-bottom: 0; border-collapse: collapse;
border-top: 1px solid white; border-bottom: 1px solid white;}
.inline-comment {margin-left: 0.15in; margin-right: 0.25in; font-size: 10pt;
padding-left: 0.20in; border-left: 3px solid LightGray; margin-top: 0;
margin-bottom: 0;}
</style>")
  (println "</head>")
  (println "<body onload=\"prettyPrint()\">"))

;;
;; This function renders the footer for the documentation page.
;;
(defn render-closing
  []
  (println "</body></html>"))