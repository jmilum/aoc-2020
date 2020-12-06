(ns aoc.core
  (:require
    [clojure.java.io :as jio]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.algo.generic.functor :as f]
    [clojure.repl :refer [apropos dir doc find-doc pst source]]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 1
;; part 1
(defn str->int [s] (when s (Integer/parseInt s)))

#_ (def data (map str->int (util/lazy-file-lines "resources/1.txt")))

#_ (first (for [x data
                y data
                :when (= 2020 (+ x y))]
            (* x y)))
;; part 2
#_ (first (for [x data
                y data
                z data
                :when (= 2020 (+ z x y))]
            (* x y z)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 2
;; part 1
(defn parse-line [line]
  (let [items (rest (re-find #"(\d+)-(\d+) ([a-z]): ([a-z]+)" line))]
    {:min  (str->int (nth items 0))
     :max (str->int (nth items 1))
     :char (nth items 2)
     :password (nth items 3)}))

(defn valid-part1? [{:keys [min max char password]}]
  (let [regex (re-pattern (str "[" char "]"))
        matches (re-seq regex password)
        char-count (count matches)]
    (and (>= char-count min) (<= char-count max))))

#_ (->> (util/lazy-file-lines "resources/2.txt")
        (map parse-line)
        (filter valid-part1?)
        (count))

;; part 2
(defn char-at [s n]
  (when (and (<= n (count s))
             (> n 0))
    (str (nth s (dec n)))))

(defn valid-part2? [{:keys [min max char password]}]
  (let [a (= char (char-at password min))
        b (= char (char-at password max))]
    (and (or a b)
         (not (and a b)))))

#_ (->> (util/lazy-file-lines "resources/2.txt")
        (map parse-line)
        (filter valid-part2?)
        (count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 3
;; part 1
(defn load-data [file]
  (-> (slurp file)
      (str/replace #"\." "0")
      (str/replace #"\#" "1")
      (str/split-lines)
      (->> (mapv #(str/split % #""))
           (f/fmap #(mapv str->int %)))))

(defn get-loc [x y data]
  (let [x' (mod x (count (first data)))]
    (-> data (nth y) (nth x'))))

(defn init-state [data right down]
  {:x 0 :y 0 :trees 0 :right right :down down :data data})

(defn update-state [state]
  (let [{:keys [x y right down trees data]} state
        x  (+ x right)
        y (+ y down)
        trees (+ trees (get-loc x y data))]
    (assoc state :x x :y y :trees trees)))

#_ (defn run [file [right down]]
     (let [data (load-data file)]
       (->> (init-state data right down)
            (iterate update-state)
            (take (/ (count data) down))
            (last)
            (:trees))))

#_ (run "resources/3.txt" [3 1])

;;part 2
#_ (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
        (map #(run "resources/3.txt" %))
        (apply *))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;day 4
;; part 1
(defn load-data [file]
  (-> (slurp file)
      (str/split #"\n\n")
      (->> (map #(str/replace % #"\n" " "))
           (map #(str/split % #" "))
           (map (fn [x] (map #(str/split % #"\:") x)))
           (map #(into {} %)))))

(defn passport-present? [passport]
  (every? passport ["ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"]))

#_ (->> (load-data "resources/4.txt")
        (filter passport-present?)
        (count))

;; part 2
(defn numeric-in-range? [s min max]
  (fn [m] (<= min (str->int (get m s)) max)))

(defn regex-match? [s re]
  (fn [m] (re-find re (get m s))))

(def byr? (numeric-in-range? "byr" 1920 2002))
(def iyr? (numeric-in-range? "iyr" 2010 2020))
(def eyr? (numeric-in-range? "eyr" 2020 2030))
(def pid? (regex-match? "pid" #"^\d{9}$"))
(def hcl? (regex-match? "hcl" #"^\#[0-9a-f]{6}$"))
(def ecl? (regex-match? "ecl" #"^amb|blu|brn|gry|grn|hzl|oth$"))

(defn hgt? [{:strs [hgt]}]
  (let [items (rest (re-find #"(\d+)(\w\w)" hgt))
        value (str->int (nth items 0 nil))
        units (nth items 1 nil)]
    (cond
      (= units "cm") (<= 150 value 193)
      (= units "in") (<= 59 value 76)
      :else false)))

(def passport-valid? (every-pred byr? iyr? eyr? pid? hcl? ecl? hgt?))

#_ (->> (load-data "resources/4.txt")
        (filter passport-present?)
        (filter passport-valid?)
        (count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 5
;; part 1

(defn init-state [s]
  (let [path (split-at 7 (map {"B" second "F" first "L" first "R" second} (str/split s #"")))]
    {:row (range 128)
     :seat (range 8)
     :row-path (first path)
     :seat-path (second path)}))

(defn prune [coll dir]
  (let [mid-point (/ (count coll) 2)]
    (dir (split-at mid-point coll))))

(defn update-state[s]
  (let [coll-key (keyword s)
        path-key (keyword (str s "-path"))]
    (fn [state]
      (let [dir (first (path-key state))
            path (rest (path-key state))
            coll (prune (coll-key state) dir)]
        (assoc state coll-key coll path-key path)))))

(defn run [state s]
  (let [n (get {"row" 8 "seat" 4} s)]
    (->> (iterate (update-state s) state)
         (take n)
         (last)
         ((keyword s))
         (first))))

(defn seat-id [s]
  (let [state (init-state s)]
    (+ (run state "seat") (* 8 (run state "row")))))

#_ (def seats
     (->> (slurp "resources/5.txt")
          (str/split-lines)
          (map seat-id)))

#_ (apply max seats)

;; part 2

#_ (->> (slurp "resources/5.txt")
        (str/split-lines)
        (map seat-id)
        (into #{})
        (set/difference (into #{} (range (apply min seats) (inc (apply max seats))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 6
;; part 1

(defn load-data [file]
  (-> (slurp file)
      (str/split #"\n\n")))

#_ (->> (load-data "resources/test6.txt")
        (map #(str/replace % #"\n" ""))
        (map #(str/split % #""))
        (map #(into #{} %))
        (map count)
        (apply +))

;; part 2

#_ (->> (load-data "resources/6.txt")
        (map str/split-lines)
        (map (fn [x] (map #(str/split % #"") x)))
        (map (fn [x] (map #(into #{} %) x)))
        (map #(apply set/intersection %))
        (map count)
        (apply +))

