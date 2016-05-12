(ns cmacs.system
  (:require [cmacs.core :refer :all]
            [cmacs.editor :refer :all]
            [cmacs.keyboard :as k]
            [clojure.core.async :refer [chan take! <!!]])
  [:import [java.awt Frame Font Color Graphics2D]
   [java.awt.event KeyListener ComponentAdapter  WindowAdapter]
   [cmacs.core Editor]])

(def all-bindings-for-mode
  (fn [editor mode]
    (loop [m mode acc (:key-bindings mode)]
      (let [p-key (:parent m)]
        (if (nil? p-key)
          acc
          (let [p-mode (p-key (:modes editor))]
            (recur p-mode (merge
                       (:key-bindings p-mode)
                       acc))))))))


(defn create-editor-frame[editor-atom window-index]
  (let [frame (proxy
                [Frame][]
                (paint [g]
                       (paint-panel ((:windows @editor-atom) window-index) g)))]
    (.addWindowListener frame (proxy[WindowAdapter][]
                                (windowActivated[e]
                                                (swap! editor-atom
                                                       set-active-window
                                                       ((:windows @editor-atom)
                                                        window-index)))))
    (.addComponentListener
      frame (proxy
              [ComponentAdapter ][]
              (componentResized [e]
                                (swap!
                                  editor-atom
                                  update-in [:windows window-index]
                                  #(do-layout-panel
                                     % 0 23
                                     (.getWidth frame)
                                     (.getHeight frame))))))
    (.setSize frame 500 500)
    (.setVisible frame true)
    (.addKeyListener
      frame
      (make-key-listener
        #(handle-key editor-atom frame %)
        (fn[] (set (keys (all-bindings-for-mode @editor-atom
                           (let [mode-key (-> @editor-atom
                                              active-view
                                              :buffer
                                              :mode)]
                             (mode-key (:modes @editor-atom)))))))))

    frame))

(defn init-keybindings[ed]
  (update ed :modes assoc
          :fundamental
          {:key-bindings
           {(k/kbd "C-x C-f")    [#'cmacs.editor/find-file]
            (k/kbd "ENTER")      [#'cmacs.editor/new-line],
            (k/kbd "LEFT")       [#'cmacs.editor/right -1],
            (k/kbd "C-o")        [#'cmacs.editor/find-file "/Users/mikey/c/cmacs/src/cmacs/editor.clj"],
            (k/kbd "DELETE")     [#'cmacs.editor/delete-forward-char],
            (k/kbd "UP")         [#'cmacs.editor/down -1],
            (k/kbd "DOWN")       [#'cmacs.editor/down],
            (k/kbd "RIGHT")      [#'cmacs.editor/right],
            (k/kbd "BACK_SPACE") [#'cmacs.editor/delete-backward-char]}}
          :minibuffer
          {:parent :fundamental,
           :key-bindings
           {(k/kbd "ENTER")      [#'cmacs.editor/minibuffer-submit]}}))

(defn create-editor[]
  (let [editor
        (->> (Editor. nil [] {})
             (add-buffer  :lines ["a"] :editor)
             (add-buffer  :lines ["b"] :editor))
        buffers (:buffers editor)
        main-panel
        (make-panel
          [(make-panel (make-view :buffer (buffers 0) :active true))
           (make-panel (make-view :buffer (buffers 1) :active true))])]
    (-> editor
        (assoc  :windows [main-panel]  :answers (chan 1000))
        init-keybindings)))


(defn start
  "Creates an Editor atom and java.awt.Frames for each of its windows"
  []
  (let [ed (atom (create-editor))
        frames  (vec (for [i (range (count (:windows @ed)))]
                       (create-editor-frame ed i)))]
    {:ed ed
     :frames frames}
    ))

(defn stop
  "Disposes of all java.awt.Frames"
  [system]
  (doseq [f (:frames system)]
    (.dispose f)
    ))
