;; # Lit-Doc
;;
;; My first stab at a literate documentation generator for Clojure
;; source code.
;;
;; __Author:__ [Christopher Miles](mailto:twitch@nervestaple.com)
;;
;; I don't remember how I stumbled on the idea of [Literate
;; Programming] (https://secure.wikimedia.org/wikipedia/en/wiki/Literate_programming)
;; but I liked the idea right off-the-bat. I had been struggling to
;; find a balance between the amount of comments I write into my code
;; and other types of documentation. This idea writing a narrative
;; into the code itself sounded like an idea worth exploring.
;;
;; This is my first attempt to write a simple literate documentation
;; generator for Clojure source code. It's not particularly clever but
;; it's a place to start.
;;
;; The functions in this file have several dependencies:
;;
;; * All logging is done with Clojure Contrib's logging functions
;;
;; * We're using Clojure Contrib's command-line functions to parse
;; arguments passed in from the shell
;;
;; * [Hiccup](https://github.com/weavejester/hiccup) provides
;; functions for outputting HTML
;;
;; * [PegDown](https://github.com/sirthias/pegdown) is used to parse
;; the comments into HTML
;;
(ns com.nervestaple.lit-doc.main
  (:gen-class)
  (:use
   [clojure.contrib.logging]
   [clojure.contrib.command-line]
   [hiccup.core]
   [com.nervestaple.lit-doc.html-template])
  (:require
   [clojure.contrib.duck-streams :as duck-streams])
  (:import
   (java.io File)
   (org.pegdown PegDownProcessor)
   (org.pegdown Parser)))

;; We want to instantiate one PegDownProcessor and then keep using the
;; one as it's expensive to create.
;;
;; We're using PegDown to transform the text of the comment into
;; HTML. Here we turn on the smart quotes, dashes, ellipse and the
;; url-to-link extensions.
(def *comment-processor* (PegDownProcessor.
                          (bit-or Parser/SMARTYPANTS Parser/AUTOLINKS)))

;; We want to read through the source files line-by-line. This
;; function takes a File as its argument and returns a list of line
;; numbers and the text of that line. The results look like...
;;
;;     ((0 "(defn my-function [arg1]")
;;      (1 "  (println arg1))"))
;;
(defn get-lines
  [source-file]
  (let [lines (line-seq (duck-streams/reader source-file))]
    (partition 2 (interleave (range 1 (inc (count lines))) lines))))

;; This function expects a line of code as its argument and returns
;; true if that line of code represents a comment. It's really
;; simplistic, it's just checking to see if the line starts with two
;; semicolons (";;").
(defn is-comment?
  [line-text]
  (let [trimmed-line (.trim line-text)]
    (if (and (< 1 (count trimmed-line))
             (= ";;" (.substring (.trim trimmed-line) 0 2)))
      true)))

;; This function returns true if the line is a comment and if it's
;; inline, that is, if it's mixed in with code.
(defn is-inline-comment?
  [line-text]
  (if (is-comment? line-text)
    (if (= \space (first line-text))
      true)))

;; This function takes a File of Clojure source code as its argument
;; and returns a data structure representing the code and the comments
;; found in that file. The data structure looks like...
;;
;;     [{:type :comment :lines ((0 "this is a comment")
;;                              (1 "this is another one"))}
;;      {:type :code    :lines ((2 "(defn myfunction [arg1]")
;;                              (3 "  (println arg1))"))}]
;;
(defn parse-file
  [source-file]

  (let [lines (get-lines source-file)]

    ;; We loop through the lines of source code one at a time and
    ;; recur with the remaining lines, a flag to indicate the type of
    ;; data we're currently parsing, a vector of the lines of data
    ;; we're currently collecting (comments or code) and a vector of
    ;; the data structure we're building.
    (loop [line-in (first lines) lines (rest lines)
           parsing nil chunk [] parsed []]
      (cond

        ;; If we're out of lines, return the parsed data
        (nil? line-in)

        ;; if we have some lines collected, add them to our parsed
        ;; data as either code or comments
        (if (< 0 (count chunk))
          (if (= :code parsing)
            (conj parsed {:type :code :lines chunk})
            (conj parsed {:type :comment :lines chunk
                          :inline (if (is-inline-comment? (second (first chunk)))
                                    true false)}))
          parsed)

        (is-comment? (second line-in))
        (recur (first lines) (rest lines) :comment
               (if (not= parsing :comment) [line-in] (conj chunk line-in))
               (if (not= parsing :comment)
                 (conj parsed {:type :code :lines chunk}) parsed))

        (not (is-comment? (second line-in)))
        (recur (first lines) (rest lines) :code
               (if (not= parsing :code) [line-in]  (conj chunk line-in))
               (if (not= parsing :code)
                 (conj parsed {:type :comment :lines chunk
                               :inline (if (is-inline-comment? (second (first chunk)))
                                         true false)}) parsed))))))

;; Here we have a simple function for rendering a line of code, this
;; will be used to build up a chunk of HTML that represents a run of
;; code. Again, not to sophisticated; it accepts a line of code and
;; then spits out the line number and the text of the line.
(defn render-code-line
  [line-in]
  (str (second line-in) "\n"))

;; This function takes a sequence of lines and emits a chunk of
;; pre-formatted HTML to standard out.
(defn render-code
  [source-lines]
  (if (< 0 (count source-lines))
    (println (html [:pre {:class (str "code prettyprint lang-lisp "
                                      ;"linenums:" (first (first source-lines))
                                      )}
                    (h (apply str (map render-code-line source-lines)))]))))

;; Rendering a line of comment is a little more complicated than code
;; but not a lot. This function accepts a line and returns either the
;; text of the comment or a line break if the line has only whitespace
;; or is empty.
;;
;; What it's doing is removing the two leading semicolons and the
;; space immediately after them (if present). We lose the line break
;; at the end when we read the text in so we add that back to the end.
(defn render-comment-line
  [line-in]
  (let [trimmed-line (.trim (second line-in))]
    (let [stripped-line (if (and (< 2 (count trimmed-line))
                              (= \space (nth trimmed-line 2)))
                       (apply str (drop 3 trimmed-line))
                       (apply str (drop 2 trimmed-line)))]
      (if (< 0 (count stripped-line))
        (str stripped-line "\n")
        "\n"))))

;; This function takes a sequence of lines and emits a chunk of HTML
;; with the contents of the comment.
(defn render-comment
  [source-lines]
  (if (< 0 (count source-lines))

    ;; We invoke PegDownProcessor to transform our comments into HTML.
    (println
     (.markdownToHtml *comment-processor*
                      (apply str (map render-comment-line source-lines))))))

;; This function wraps inline comments in a div so that we can style
;; them all fancy like.
(defn render-inline-comment
  [source-lines]
  (println "<div class=\"inline-comment\">")
  (render-comment source-lines)
  (println "</div>"))

;; This function is looking a destination File for writing the HTML
;; documentation and the source data that is returned by the
;; parse-file function.
(defn render-file
  [dest-file parsed-file]

  ;; We're going to delete the destination file and replace it with a
  ;; brand new file.
  (if (.exists dest-file)
    (.delete dest-file))
  (.createNewFile dest-file)

  ;; with-out-writer lets us capture anything written to standard out
  ;; and writes it to the destination file instead.
  (duck-streams/with-out-writer dest-file

    (render-heading)

    ;; We map over the parsed data structure and emit either code or
    ;; comments.
    (dorun
     (map (fn [code-in]
            (cond
              
              (= :code (:type code-in))
              (dorun (render-code (:lines code-in)))

              (and (= :comment (:type code-in)) (:inline code-in))
              (dorun (render-inline-comment (:lines code-in)))
              
              (= :comment (:type code-in))
              (dorun (render-comment (:lines code-in)))))
          parsed-file))

    (render-closing)))

;; This function takes two File objects as it's arguments, the
;; destination directory for the documentation and a source file to
;; process. It munges the name of the source file into one long
;; filename to prevent name clashes and returns a new File object.
(defn munge-file-name
  [dest-dir source-file]
  (File. dest-dir
         (str (apply str
                     (interpose "_" (rest (.split (.getPath source-file) "/"))))
              ".html")))

;; This function takes a File object that points to the directory
;; containing the source code as its argument. It will return a list
;; of all of the Clojure source code files that it can find, it will
;; search sub-directories recursively.
(defn get-paths
  [source-dir]

  ;; We filter the resulting files so that we only return those ending
  ;; with ".clj".
  (filter (fn [path-in]
            (and (< 4 (count (.getName path-in)))
                 (= ".clj" (apply str (take-last 4 (.getName path-in))))))

          ;; Our recursive calls to get-paths builds a nested
          ;; sequence, we want to flatten it into one list.
          (flatten

           ;; We check each file in the directory, if it's a directory
           ;; we recurse for its contents otherwise it's a file and we
           ;; emit its File.
           (map (fn [path]
                  (if (.isDirectory path)
                    (get-paths path)
                    path))
                (.listFiles source-dir)))))

;; This function takes two String objects as its arguments: this first
;; is the path to the Clojure source files, then second is the path
;; were the documentation will be stored. It then reads through all of
;; the source code and writes out all of the documentation.
(defn process-source
  [source dest]

  ;; We need File representations of both paths
  (let [source-dir (File. source)
        dest-dir (File. dest)]

    ;; We make sure the source directory exists and is read-able.
    (if (not (.isDirectory source-dir))
      (throw (Exception. (str "\"" source-dir "\" is not a directory"))))

    (if (not (.canRead source-dir))
      (throw (Exception. (str "Cannot read from \"" source-dir "\""))))

    ;; We create the destination if it doesn't exist
    (if (not (.exists dest-dir))
      (.mkdirs dest-dir))

    ;; We double-check to make sure the destination directory is a
    ;; url-to-link extensionsdirectory and is write-able.
    (if (not (.isDirectory dest-dir))
      (throw (Exception. (str "\"" dest-dir "\" is not a directory"))))

    (if (not (.canWrite dest-dir))
      (throw (Exception. (str "Cannot write to \"" dest-dir "\""))))

    ;; We map over all of the files in the source directory.
    (map (fn [source-path]

           ;; We get a munged name to hold the documentation, the
           ;; parsed data for the source file and then we render our
           ;; documentation.
           (render-file (munge-file-name dest-dir source-path)
                        (parse-file source-path)))
         (get-paths source-dir))))

;; This is the main entry point to our documentation generator, it
;; handles arguments when called from the command line. It will parse
;; out the arguments and try to generate the documentation.
(defn -main
  "Provides the main function needed to bootstrap the application."
  [& args]

  (with-command-line args
    (str "Generate literate programming documentation for Clojure code.\n"
         "Usage: -s <dir> -d <dir>")
    [[dest d "Destination directory for HTML output" "lit-doc"]
     [source s "Directory containing source code" "src"]]
    (try
      (process-source source dest)
      (catch Exception exception
        (warn exception)))))

;; This is a convenience function that lets us call the hidden main
;; function when we're testing.
(defn main
  [& args]
  (apply -main args))