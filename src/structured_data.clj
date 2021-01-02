(ns structured-data)

(defn do-a-thing [x]
  :-)

(defn spiff [v]
  (cond
    (not (vector? v)) nil
    (< (count v) 3)   nil
    :else (+ (get v 0) (get v 2))))

(defn cutify [v]
  (let [xx "<3"]
    (if (not (vector? v))
      ["<3"]
      (conj v "<3"))))

(defn spiff-destructuring [v]
  (cond
    (not (vector? v)) nil
    (< (count v) 3) nil
    :else (let [[x y z] v]
            (+ x z))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [ x2 y2]] rectangle
    [x y] point]
    (cond
      (and (<= x1 x x2) (<= y1 y y2)) true
      :else false)))

(defn contains-rectangle? [outer inner]
  (let [[ip1 ip2] inner]
    (and (contains-point? outer ip1)
         (contains-point? outer ip2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors
         (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ff (fn [x] (get x 1))]
    (map ff collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply >= a-seq) true
    (apply <= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n '*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors
         (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [an
        (fn [book] (:authors book))]
    (apply clojure.set/union (map an books))))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn author->string [author]
  (let [an (:name author)
        aby (:birth-year author)
        ady (:death-year author)]
    (cond
      (not (nil? aby))
        (if (not (nil? ady))
          (str an " (" aby " - " ady ")")
          (str an " (" aby " - )"))
      :else an)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bt (:title book)
        ba (:authors book)]
    (str bt ", written by" (authors->string ba))))

(defn books->string [books]
  (let [bc (count books)
        s (if (>= (count books) 2)
            "s"
            "")]
    (if (== 0 bc)
      "No books."
      (str bc " book" s ". "
           (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
  (filter (fn [x] (= (:name x) name)) authors))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [dx (+ x x)]
    (Math/pow dx dx)))

(defn sum-pairs [first-pair second-pair]
  [(+ (first  first-pair) (first  second-pair))
   (+ (second first-pair) (second second-pair))])

(defn sum-pairs-2 [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(def china {:name "China Mieville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Fellisen"})
(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})
(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})
(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(def books [cities, wild-seed, embassytown, little-schemer
            silmarillion, deus-irae])

(defn author-names [book]
  (map :name (:authors book)))

(defn mungefy [a-seq]
  (let [munge (fn [x] (+ x 42))]
    (map munge a-seq)))

