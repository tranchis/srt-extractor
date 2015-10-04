(ns srt-extractor.core
  (:require [clojure.data.xml :refer :all]))

(def language "spanish")
(def output-file (str "/Users/sergio/Desktop/" language ".txt"))
(def output-srt (str "/Users/sergio/Desktop/" language ".srt"))
(def file (str "/Users/sergio/Desktop/" language " subts dvd Clip.fcpxml"))
#_(def frame-rate 23.976)
(def frame-rate 24)
(def base-rate 1000/1001)

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

(defn xml->titles []
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

(defn write-plain! [output-file]
  (let [ts (xml->titles)]
    (spit output-file "" :append false)
    (dorun (map #(spit output-file (str (timecode (:start %)) " "
                                        (timecode (:end %)) " "
                                        (:text %) "\n") :append true)
                ts))))

(defn write-srt! [output-file]
  (let [ts (xml->titles)]
    (spit output-file "" :append false)
    (dorun (map-indexed
            (fn [idx t]
              (spit output-file (str (inc idx) "\n"
                                     (srt-timecode (:start t)) " --> "
                                     (srt-timecode (:end t)) "\n"
                                     (:text t) "\n\n") :append true))
            ts))))

(write-plain! output-file)
(write-srt! output-srt)

#_(xml->titles)
 
