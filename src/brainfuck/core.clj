;; Clojure Brainfuck(http://en.wikipedia.org/wiki/Brainfuck)

(ns brainfuck.core)

(defn- cal-bracket-map [program]
  (letfn [(cal-brackets [idx bracket-map unmatched-brackets]
            (if (>= idx (count program))
              [bracket-map unmatched-brackets]
              (let [curr (nth program idx)
                    [new-unmatched-brackets new-bracket-map]
                    (condp = curr
                      \[ [(conj unmatched-brackets idx) bracket-map]
                      \] (if (seq unmatched-brackets)
                           [(drop-last unmatched-brackets)
                            (assoc bracket-map (last unmatched-brackets) idx
                                   idx (last unmatched-brackets))]
                           (throw (IllegalArgumentException. "unmatched ']' detected")))
                      [unmatched-brackets bracket-map])]
                (recur (inc idx) new-bracket-map new-unmatched-brackets))))]
    (let [[bracket-map unmatched-brackets] (cal-brackets 0 {} [])]
      (when (seq unmatched-brackets)
        (throw (IllegalArgumentException. "unmatched '[' detected")))
      bracket-map)))

(defn parse [program]
  (let [program (filter #{\> \< \+ \- \. \, \[ \]} program)
        bracket-map (cal-bracket-map program)]
    (loop [idx 0 tape {} pc 0]
      (if (< idx (count program))
        (condp = (nth program idx)
          \+ (recur (inc idx) (assoc tape pc (inc (get tape pc 0))) pc)
          \- (recur (inc idx) (assoc tape pc (dec (get tape pc 0))) pc)
          \. (do
               (.print System/out (char (tape pc)))
               (recur (inc idx) tape pc))
          \, (let [ch (.read System/in)
                   ch (int ch)]
               (recur (inc idx) (assoc tape pc ch) pc))
          \[ (if (pos? (tape pc))
               (recur (inc idx) tape pc)
               (recur (bracket-map idx) tape pc))
          \] (if (pos? (tape pc))
               (recur (bracket-map idx) tape pc)
               (recur (inc idx) tape pc))
          \> (recur (inc idx) tape (inc pc))
          \< (recur (inc idx) tape (dec pc)))))))

;; Hello World!
(comment
  (parse "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
  (parse "++++.")
  (parse "++-.")
  (parse "++++++++++[>+++++++<-]>++.")
  (parse "++[>++<-]]")
  (parse "++[>++<-]["))


(defn -main [file-path]
  (parse (slurp file-path)))
