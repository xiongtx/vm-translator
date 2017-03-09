(ns vm-translator.core
  (:refer-clojure :exclude [pop])
  (:require
   [clojure.java.io :as io]
   [me.raynes.fs :as fs]
   [superstring.core :as str])
  (:import
   (java.io File))
  (:gen-class))

(def label-number (atom 0))

(def combine-top-two
  "Instructions to combine two values off the stack.

  The top value is in D, the one-from-top value in M. SP is decremented."
  ["@SP"
   "AM=M-1"
   "D=M"
   "A=A-1"])

(def top-value
  "Get top value from stack.

  SP remains the same."
  ["@SP"
   "A=M-1"])

(def ^:private compare-commands
  {:eq "D;JEQ"
   :lt "D;JLT"
   :gt "D;JGT"})

(defn- parse-compare
  "EQ, LT, or GT"
  [comparison]
  (swap! label-number inc)
  (let [true-label (str "TRUE$" @label-number)
        end-label (str "END$" @label-number)]
    (concat combine-top-two
            `["D=M-D"
              ~(str "@" true-label)
              ~(get compare-commands comparison)

              ;; x != y
              ~@(conj top-value "M=0")
              ~(str "@" end-label)
              "0;JMP"

              ;; x = y
              ~(str "(" true-label ")")
              ~@(conj top-value "M=-1")

              ~(str "(" end-label ")")])))

(defn arithmetic
  [command]
  (case command
    :add (conj combine-top-two "M=D+M")
    :sub (conj combine-top-two "M=M-D")
    :neg (conj top-value "M=-M")
    :eq (parse-compare :eq)
    :gt (parse-compare :gt)
    :lt (parse-compare :lt)
    :and (conj combine-top-two "M=D&M")
    :or (conj combine-top-two "M=D|M")
    :not (conj top-value "M=!M")))

(def segment-addresses
  {:local "@LCL"
   :argument "@ARG"
   :this "@THIS"
   :that "@THAT"})

(def push-d
  ["@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def same-segments
  "Segments that are treated the same."
  #{:local :argument :this :that})

(defn- segment-dispatch
  "Dispatching fn for push/pop segments."
  [segment i & _]
  (if (contains? same-segments segment)
    same-segments
    segment))

(defmulti push segment-dispatch)

(defmethod push :constant
  ([segment i]
   `[~(str "@" i)
     "D=A"
     ~@push-d])
  ([segment i file]
   (push segment i)))

(defmethod push same-segments
  ([segment i]
   `[~(str "@" i)
     "D=A"
     ~(segment segment-addresses)
     "A=D+M"
     "D=M"
     ~@push-d])
  ([segment i file]
   (push segment i)))

(defmethod push :temp
  ([segment i]
   `[~(str "@" (+ 5 i))
     "D=M"
     ~@push-d])
  ([segment i file]
   (push segment i)))

(defmethod push :pointer
  ([segment i]
   `[~(str "@" (+ 3 i))
     "D=M"
     ~@push-d])
  ([segment i file]
   (push segment i)))

(defmethod push :static
  [segment i file]
  {:pre [(not (nil? file))]}
  `[~(str "@" file "." i)
    "D=M"
    ~@push-d])

(def pop-d
  "Assign D the top stack value.

  SP is decremented."
  ["@SP"
   "AM=M-1"
   "D=M"])

(defmulti pop segment-dispatch)

(defmethod pop same-segments
  ([segment i]
   `[~(str "@" i)
     "D=A"
     ~(segment segment-addresses)
     "D=D+M"

     ;; Store location in @R13
     "@R13"
     "M=D"

     ;; Pop stack
     ~@pop-d
     "@R13"
     "A=M"
     "M=D"])
  ([segment i file]
   (pop segment i)))

(defmethod pop :temp
  ([segment i]
   `[~@pop-d
     ~(str "@" (+ 5 i))
     "M=D"])
  ([segment i file]
   (pop segment i)))

(defmethod pop :pointer
  ([segment i]
   `[~@pop-d
     ~(str "@" (+ 3 i))
     "M=D"])
  ([segment i file]
   (pop segment i)))

(defmethod pop :static
  [segment i file]
  {:pre [(not (nil? file))]}
  `[~@pop-d
    ~(str "@" file "." i)
    "M=D"])

(defn memory-access
  ([command segment i]
   (memory-access command segment i nil))
  ([command segment i file]
   (case command
     :push (push segment i file)
     :pop (pop segment i file))))

(def arithmetic-commands
  #{:add :sub :neg :eq :gt :lt :and :or :not})

(def memory-access-commands
  #{:push :pop})

(defn comment?
  "Return true if line is a comment."
  [^String line]
  (str/starts-with? (str/triml line) "//"))

(defn ignored?
  "Return true if line should be ignored."
  [^String line]
  (or (str/blank? line)
      (comment? line)))

(defn parse-line
  [^String file ^String line]
  (when-not (ignored? line)
    (let [[command segment i] (str/split line #"\s+")
          command (keyword command)
          segment (keyword segment)
          i (when i (Integer/parseInt i))]
      (condp contains? command
        arithmetic-commands (arithmetic command)
        memory-access-commands (memory-access command segment i (fs/name file))))))

(defn parse-file
  [^String file]
  (with-open [r (io/reader file)]
    (let [results (atom [])]
      (doseq [line (line-seq r)]
        (swap! results concat (parse-line file line)))
      @results)))

(defn -main
  "Translate `vm` files into `.asm` files.

  The `.asm` files are placed in the same directory as the `.vm` files."
  [& args]
  (doseq [file args]
    (reset! label-number 0)
    (let [outfile (str (fs/parent file) File/separator (fs/name file) ".asm")]
      (with-open [w (io/writer outfile)]
        (doseq [line (parse-file file)]
          (.write w line)
          (.write w "\n"))))))
