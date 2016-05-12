(ns cmacs.editor
  (:require [cmacs.core :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as w]
            [cmacs.keyboard :as k]
            [clojure.string :as str]
            [clojure.core.async :refer [put!]])
  (:import [cmacs.core Buffer]))



(defn new-line[view]
  (let [view (restrict-col view)
        [before after] (split-at (:line view) (lines view))
        current-line (current-line-text view)
        a (.substring current-line 0 (:col view))
        z (.substring current-line  (:col view))
        new-lines (vec (concat before  (conj (rest after) z a)))
        new-buf (assoc (:buffer view) :lines new-lines)
        ]
    (assoc view :buffer new-buf :col 0 :line (inc (:line view)))))

(defn down
  ([view] (down view 1))
  ([view n]
   (let [dest
         (+ (:line view) n)
         max-line (dec (count (lines view)))
         dest (cond
                (> dest max-line) max-line
                (< dest 0) 0
                :else dest)]
     (assoc view :line dest))))

(defn end-of-line[v]
  (assoc v :col (.length (current-line-text v))))
(defn begin-of-line[v]
  (assoc v :col 0))


(defn right
  ([view] (right view 1))
  ([view n]
   (let [view (restrict-col view)
         dest (+ (:col view) n)
         max-col (.length (current-line-text view))]
     (cond
       (< dest 0) (if (pos? (:line view)) (end-of-line (down view -1)) view)
       (> dest max-col) (if (< (:line view) (dec (count (lines view))))
                          (begin-of-line (down view 1))
                          view)
       :else (assoc view :col dest)))))

(defn join-lines [view]
  (let [[before after] (split-at (:line view) (lines view))
        new-line (str (first after) (second after))
        new-lines (vec (concat before (cons new-line (drop 2 after))))
        ]
    (assoc-in view [:buffer :lines] new-lines)))


(defn delete-forward-char[old-view]
  (let [view (restrict-col old-view)]
    (if (:read-only (properties view))
      (error old-view "buffer is read-only here")
      (if (= (:col view)  (.length  (current-line-text view)))
        (join-lines view)
        (let [current-line (current-line-text view)
              a (.substring current-line 0 (:col view))
              z (.substring current-line  (inc (:col view)))
              new-line (str a z)
              new-lines (assoc (lines view) (:line view) new-line)
              new-buf (assoc (:buffer view) :lines new-lines)]
          (assoc view :buffer new-buf ))))))

(defn delete-backward-char[view]
  (if (every? zero? ((juxt :col :line) view))
    view
    (if (:read-only (properties view))
      (error view "buffer is read-only here")
      (delete-forward-char(right view -1)))))

(defn file-into-vec "loads a the lines of a file into a vector"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn ^:editor find-file[ed ^String file]
  (let [ed (add-buffer :editor ed :name file  :lines (file-into-vec file) :path file)
        buf (peek (:buffers ed))
        view (active-view ed)]
    (replace-view ed view (assoc view :buffer buf))))

(defn ^:editor minibuffer-submit[ed]
  (let [buf (->> ed
                 active-view
                 :buffer)
        lines (->> buf
                   :lines
                   (str/join \newline))]
    (let [answer (.substring lines (-> buf :prompt .length))]
      ;need to close minibuffer
      (put! (:answers ed) answer))
    ed))




;need to make functions into commands by allowing them to be interactive
;when an interactive command is called it sends the buffer into a state
;in which it knows what questions to ask and what to do with the answers.
;Each time
