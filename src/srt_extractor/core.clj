(ns srt-extractor.core
  (:require [clojure.data.xml :refer :all]))

(def language "spanish")
#_(def frame-rate 23.976)
(def frame-rate 24)
#_(def base-rate 1000/1001)
(def base-rate 1000/1000)


(defn output-file [language]
  (str "/Users/sergio/Desktop/" language ".txt"))

(defn output-srt [language]
  (str "/Users/sergio/Desktop/" language ".srt"))

(defn file [language]
  (str "/Users/sergio/Desktop/" language " subts dvd Clip.fcpxml"))

(defn number-to-string [n]
  (if (< n 10)
    (str "0" n)
    (str n)))

(defn decimal-to-string [d]
  (let [n (* 1000 d)]
    (if (< n 10)
      (str "00" n)
      (if (< n 100)
        (str "0" n)
        (str n)))))

(defn srt-timecode [tc]
  (let [t (* base-rate tc)
        int-part (int t)
        dec-part (mod t 1)
        hours (int (/ int-part 3600))
        minutes (int (/ (- int-part (* 3600 hours)) 60))
        seconds (- int-part (* 3600 hours) (* 60 minutes))]
    (str (number-to-string hours) ":" (number-to-string minutes) ":"
         (number-to-string seconds) ","
         (subs (decimal-to-string dec-part) 0 3))))

(defn timecode [tc]
  (let [t (* base-rate tc)
        int-part (int t)
        dec-part (mod t 1)
        frames (int (* dec-part frame-rate))
        hours (int (/ int-part 3600))
        minutes (int (/ (- int-part (* 3600 hours)) 60))
        seconds (- int-part (* 3600 hours) (* 60 minutes))]
    (str (number-to-string hours) ":" (number-to-string minutes) ":"
         (number-to-string seconds) ":" (number-to-string frames))))

(defn time-converted [st]
  (double (read-string (subs st 0 (dec (count st))))))

(defn title-processed [t initial-offset]
  (let [texts (filter #(= (:tag %) :text) (:content t))
        text (map #(first (:content (first (:content %)))) texts)
        clean (remove #(or (nil? %) (= "" %)) text)
        trimmed (map clojure.string/trim clean)
        formatted-text (clojure.string/join "\n" clean)
        offset (time-converted (:offset (:attrs t)))
        duration (time-converted (:duration (:attrs t)))
        start (time-converted (:start (:attrs t)))
        dependent (filter #(= (:tag %) :title) (:content t))
        real-start (- offset initial-offset)
        children (mapcat #(title-processed % start) dependent)
        tc-children (map (fn [c]
                           (merge c
                                  {:start (+ (:start c) real-start)
                                   :end (+ (:end c) real-start)}))
                         children)]
    (remove
     #(= "" (:text %))
     (concat [{:text formatted-text
               :enabled (:enabled (:attrs t))
               :start real-start
               :end (- (+ offset duration) initial-offset)}]
             tc-children))))

(defn xml->titles [file]
  (let [input-xml (java.io.FileReader. (java.io.File. file))
        xml (parse input-xml)
        content (:content xml)
        resources (first (filter #(= (:tag %) :resources) content))
        medias (filter #(= (:tag %) :media) (:content resources))
        sequence (first
                  (map (fn [m] (first (:content m)))
                       (filter #(= (:tag (first (:content %)))
                                   :sequence) medias)))
        spine (first (filter #(= (:tag %) :spine) (:content sequence)))
        timeline (:content spine)
        titles timeline #_(filter #(= (:tag %) :title) timeline)]
    (remove (fn [t] (= "0" (:enabled t))) (mapcat #(title-processed % 0) titles))))

(defn write-plain! [output-file ts]
  (spit output-file "" :append false)
  (dorun (map #(spit output-file (str (timecode (:start %)) " "
                                      (timecode (:end %)) " "
                                      (:text %) "\n") :append true)
              ts)))

(defn write-srt! [output-file ts]
  (spit output-file "" :append false)
  (dorun (map-indexed
          (fn [idx t]
            (spit output-file (str (inc idx) "\n"
                                   (srt-timecode (:start t)) " --> "
                                   (srt-timecode (:end t)) "\n"
                                   (:text t) "\n\n") :append true))
          ts)))

#_(write-plain! output-file)
#_(write-srt! output-srt)

(defn find-closer [t sync]
  (let [diffs (map #(hash-map :diff (+ (Math/abs (- (:start %) (:start t)))
                                       (Math/abs (- (:end %) (:end t))))
                              :target %) sync)
        choice (first (sort-by :diff diffs))]
    (merge t {:start (:start (:target choice))
              :end (:end (:target choice))})))

(defn synchronized [sync to-sync]
  (map #(find-closer % sync) to-sync))

(defn test []
  (write-plain! (output-file "english")
                (synchronized
                 (xml->titles (file "spanish"))
                 (xml->titles (file "english"))))

  (write-srt! (output-srt "english")
              (synchronized
               (xml->titles (file "spanish"))
               (xml->titles (file "english"))))

  (write-plain! (output-file "german")
                (synchronized
                 (xml->titles (file "spanish"))
                 (xml->titles (file "german"))))

  (write-srt! (output-srt "german")
              (synchronized
               (xml->titles (file "spanish"))
               (xml->titles (file "german"))))

  (write-plain! (output-file "german-extended")
                (synchronized
                 (xml->titles (file "spanish"))
                 (xml->titles (file "german-extended"))))

  (write-srt! (output-srt "german-extended")
              (synchronized
               (xml->titles (file "spanish"))
               (xml->titles (file "german-extended"))))

  (write-plain! (output-file "spanish")
                (xml->titles (file "spanish")))

  (write-srt! (output-srt "spanish")
              (xml->titles (file "spanish"))))
