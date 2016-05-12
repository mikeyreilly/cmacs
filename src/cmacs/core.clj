(ns cmacs.core
  (:require [clojure.pprint :as pp]
            [clojure.walk :as w]
            [cmacs.keyboard :as k]
            [clojure.core.async :refer [chan take! <!!]])
  [:import [java.awt Frame Font Color Graphics2D]
   [java.awt.event KeyListener ComponentAdapter  WindowAdapter]])

(defrecord Buffer [name lines path id properties mode])

(defn add-buffer
  [& {:keys [prompt editor name lines path mode]
      :or {name "*Scratch*"
           mode :fundamental}}]
  (let [buffers (:buffers editor)
        id (if (seq buffers)
             (inc (apply max (map :id buffers)))
             0)]
    (update-in editor [:buffers] conj (map->Buffer
                                        {:name name :lines lines
                                         :path path :id id
                                         :properties [] :mode mode
                                         :prompt prompt}))))

(defrecord View[col line width-ch height-ch
                v-scroll h-scroll font char-width
                char-height active buffer])
(defn make-view
  [& {:keys [col line width-ch
             height-ch v-scroll h-scroll font
             char-width char-height  buffer]
      :or {col 0
           line 0
           v-scroll 0
           h-scroll 0
           font (Font/decode "Courier New-14")
           char-width 8
           char-height 14
           lines []}}]
  (View. col line width-ch
         height-ch v-scroll h-scroll font
         char-width char-height 0 buffer))

(defrecord Panel[x y width-px height-px contents right])

(defn make-panel [contents & right]
  {:pre  [contents]}
  (map->Panel {:contents contents :right right}))

(defrecord Editor[windows buffers modes])

(defn panel-seq [panel]
  (tree-seq #(not (instance? View (:contents %)))
            :contents  panel))

(defn panel-views[panel]
  (map :contents (filter
                  (fn[v](instance? View (:contents v))) (panel-seq panel))))

(defn views[editor]
  (mapcat panel-views (:windows editor)))

(defn active[views]
  (let [active (apply max-key :active views)]
    (if active active (first views))))

(defn active-window[editor]
  (active (:windows editor)))

(defn active-view[editor]
  (active (panel-views (active-window editor))))

(defn set-active-window[editor window]
  (let [old-active (active-window editor)]
    (if (= window old-active)
      editor
      (w/postwalk-replace
        {window (assoc window :active ((fnil inc 0) (:active old-active)))}
        editor))))

(defn window-of[editor view]
  ((comp :window first)
   (filter #(contains? (:panels %) view)
           (map
            (fn[x] {:window x :panels (set (panel-views x))})
            (:windows editor)))))

(defn set-active-view
  "makes the specified view the only active
  view in the window which contains it"
  [editor view]
  {:pre [(and (instance? Editor editor)
              (instance? View view))]}
  (let [old-active (active (panel-views (window-of editor view)))]
    (if old-active
      (w/postwalk-replace
       {view (assoc view :active ((fnil inc 0) (:active old-active)))}
       editor)
      (throw (IllegalArgumentException. "old-active is nil")))))

(defn lines[view]
  (get-in view [:buffer :lines]))

(defn current-line-text
  [view]
  (let [buf (lines view)]
    (if (seq buf)
      (buf (:line view))
      "")))

(defn replace-view[editor old-view new-view]
  {:pre [(and (instance? View old-view)
              (instance? View new-view))]}
  (let [old-buffer (:buffer old-view)
        new-buffer (:buffer new-view)]
    (w/postwalk-replace
      {old-buffer new-buffer}
      (w/postwalk-replace
        {old-view new-view}
        editor))))

(defn update-view[editor f & args]
  (let [view (active-view editor)]
    (replace-view editor view (apply f view args))))

(defn restrict-col[view]
  (let [c (:col view)
        len (.length (current-line-text view))]
    (if (> c len)
      (assoc view :col len)
      view)))

(defn scroll[x]
  (if (instance? Editor x)
    (let [v (active-view x)]
      (replace-view x v (scroll v)))
    (let [view x]
      (let [[width-ch height-ch v-scroll h-scroll col line]
            ((juxt
              :width-ch :height-ch :v-scroll
              :h-scroll :col :line) view)]
        (let [view (cond (< line v-scroll)
                         (assoc view :v-scroll line)
                         (> line (+ v-scroll height-ch -1))
                         (assoc view :v-scroll
                                (max 0 (- line height-ch -1)))
                         :else view)]
          (cond
            (< col h-scroll) (assoc view :h-scroll col)
            (> col  (+ h-scroll width-ch -1))
            (assoc view
                   :h-scroll (max 0 (- col width-ch -1)))
            :else view))))))


(defn properties
  "returns the set of properties that apply at line, col"
  ([view] (properties view (:line view) (:col view)))
  ([view line col]
   (set (map second (filter
                      (fn[p] (let [ [{begin  :begin  end :end} _] p]
                               (and (<= (compare begin [line col]) 0)
                                    (<= (compare [line col] end) 0 ))))
                      (:properties (:buffer view)))))))

(defn error[view message]
  ;don't just print out
  (println "error: " message)
  view)

(defn insert-char[view c]
  (if (:read-only (properties view))
    (error view "buffer is read-only here")
    (let [view (restrict-col view)
          current-line (current-line-text view)
          a (.substring current-line 0 (:col view))
          z (.substring current-line  (:col view))
          new-line (str a c z)
          new-lines (assoc (lines view) (:line view) new-line)
          new-buf (assoc (:buffer view) :lines new-lines)]
      (assoc view :buffer new-buf :col (inc (:col view))))) )

(defn action->fn[action-def missing]
  (let [[action-var & action-parameters] action-def]
    (let [action-parameters (concat action-parameters missing)
          action (var-get action-var)
          action (if (:editor (meta action-var))
                   (if action-parameters
                     (fn[editor](apply action editor action-parameters))
                     action)
                   (if action-parameters
                     (fn[editor](apply update-view editor action action-parameters))
                     (fn[editor](update-view editor action))))]
      action)))

(defn lookup-action
  ([ed ks] (lookup-action ed ks (-> ed
                                    active-view
                                    :buffer
                                    :mode)))
  ([ed ks mode]
   (let [r (get-in ed [:modes mode :key-bindings ks])]
     (if r r (if-let [parent (get-in ed [:modes mode :parent])]
               (recur ed ks parent)
               nil)))))

(defn missing-params [action-def]
  (let [[action-var & action-parameters] action-def]
    (let [args (apply min-key count (:arglists (meta action-var)))
          len  (inc (count action-parameters))]
      (if (< len (count args))
        (subvec args len)
        []))))

(defn add-text-properties [buffer begin end property]
  (update-in buffer [:properties] conj [{:begin begin :end end} property]))

(defn do-layout-panel [ panel x y width-px height-px]
  {:pre  [(:contents panel)]}
  (let [contents (:contents panel)
        right (:right panel)]
    (if (instance? View contents)
      (assoc panel :x x :y y :width-px width-px :height-px height-px
        :contents (assoc contents :width-ch (quot width-px (:char-width contents))
                    :height-ch (quot height-px (:char-height contents))))
      (let [cnt (count contents)
            child-width (if right (quot width-px cnt) width-px)
            child-height (if right height-px (quot height-px cnt))
            dx (if right child-width 0)
            dy (if right 0 child-height)]
        (assoc panel :x x :y y :width-px width-px :height-px height-px
          :contents (vec (for [i (range cnt) :let [p (contents i)]]
                           (do-layout-panel  p
                                             (+ x (* dx i))
                                             (+ y (* dy i))
                                             child-width child-height))))))))

(defn ^:editor read-from-minibuffer [editor prompt default-value]
  (let [prompt (str prompt \space)
        editor (add-buffer :prompt prompt :name "minibuffer" :lines [prompt] :editor editor :mode :minibuffer)
        buf (add-text-properties (peek (:buffers editor)) [0 0] [0 (.length prompt)] :read-only)
        was-active (active-window editor)
        new-window (do-layout-panel (make-panel [was-active
                                                 (make-panel (make-view :col (inc (.length prompt)) :buffer buf))])
                                    (:x was-active)
                                    (:y was-active)
                                    (:width-px was-active)
                                    (:height-px was-active))

        minibuffer-view ((comp :contents peek :contents) new-window)
        editor (w/postwalk-replace {was-active new-window} editor)
        ]
    (set-active-view editor  minibuffer-view)))


(defn ask[prompt editor-atom frame]
  (let [old-ed @editor-atom]
    (swap! editor-atom read-from-minibuffer prompt "")
    (.repaint frame)
    (let [answer   (<!! (:answers @editor-atom))]
      (reset! editor-atom old-ed)
      answer)))

;need to check if action we're looking up needs to ask for params
(defn handle-key[editor-atom frame k]
  (prn "k" k)
  (if (char? k)
    (do
      (swap! editor-atom #(scroll (update-view % insert-char k)))
      (.repaint frame)
      )
    (let [action-def (lookup-action @editor-atom k)]
      (when action-def
        (future
          (try
            (swap! editor-atom
                   (comp scroll
                         (action->fn action-def
                                     (vec (for [p (missing-params action-def)]
                                            (ask p editor-atom frame))))))

            (.repaint frame)
            (catch Exception ex (.printStackTrace ex))))))))

(defn make-key-listener
  "if part of a key sequence is typed wait until whole
  sequence or not a sequence is typed
  before putting on ch

  f is a function that handles key sequences
  key-sequences is a function which returns a set of the key-sequences
  we care about (in the active mode)"
  [f key-sequences]
  (let [key-sequence (atom [])
        is-modifier? (fn[e] (#{0 16 17 18 20 157} (.getKeyCode e)))
        matches (fn[thing key-sequences]
                  (let [len (count thing)]
                    (some #(and
                             (>= (count %) len)
                             (= thing (subvec % 0 len)))
                          key-sequences)))
        done (fn[](and (pos? (count @key-sequence))(not (matches @key-sequence (key-sequences)))))]
    (reify KeyListener
      (keyPressed [this e]
                  (prn "e")
                  (when-not (is-modifier? e)
                    (swap! key-sequence conj (k/KeyEvent->KeyStroke e))
                    (when ((key-sequences) @key-sequence)
                      (let [old-ks @key-sequence]
                        (reset! key-sequence [])
                        (f old-ks)))))

      (keyTyped [this e]
                (prn "e")
                (when (done)
                  (let [old-ks @key-sequence]
                    (reset! key-sequence [])
                    (f (if (= (count old-ks) 1)
                         (.getKeyChar e)
                         old-ks)))))

      (keyReleased [this e]
                   (prn "e")
                   (when (done)
                     (let [old-ks @key-sequence]
                       (reset! key-sequence [])
                       (f old-ks)))))))


(defn pixel-position[view col line]
  {:x (* (- col (:h-scroll view)) (:char-width view))
   :y (* (- line (:v-scroll view)) (:char-height view))})

(defn paint-view[view g x y width-px height-px]
  (.setFont g (:font view))
  (.setColor g Color/WHITE)
  (.fillRect g x y width-px height-px)
  (.setColor g Color/LIGHT_GRAY)
  (let [c (min (:col view) (.length (current-line-text view)))
        pix (pixel-position view c (:line view))]
    (.fillRect g
               (+ x (pix :x))
               (+ y (pix :y))
               (:char-width view)
               (:char-height view)))
  (.setColor g Color/BLACK)
  (let [lines (lines view)]
    (doall (for [i (range (count lines))]
             (let [pix (pixel-position view 0 i)]
               (.drawString g (lines i) (+ x (pix :x)) (+ y (:char-height view) (pix :y))))))))

(defn paint-panel [panel g]
  (let [contents (:contents panel)
        right (:right panel)]
    (if (instance? View contents)
      (paint-view contents g (:x panel)
                  (:y panel) (:width-px panel) (:height-px panel))
      (doseq [i (range (count contents)) ]
        (paint-panel (contents i) g)))))
