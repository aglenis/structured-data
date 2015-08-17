(ns structured-data)

(defn do-a-thing [x]
  (let  [xt2 (+ x x)]
    (Math/pow xt2 xt2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)
    )
  )

(defn square? [rectangle]
  (== (height rectangle)(width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle)(width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2) ))
  )

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2)))
  )

(defn count-field [field book]
  (count (field book))
  )

(defn title-length [book]
  (count-field :title book)
  )

(defn author-count [book]
  (count-field :authors book)
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author )
        new-book (assoc book :authors new-authors)]
    new-book)
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [collection] (get collection 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (vec a-seq) (distinct (vec a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
    )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
 (set (apply concat (map :authors (map old-book->new-book books))
  )))

(defn all-author-names [books]
  (set(map :name (authors books)))
  )


(defn author->string [author]
  (let [author-name (:name author)
        author-birth (:birth-year author "")
        author-death (:death-year author "")
        plate-date (cond
                    (= author-birth author-death "") ""
                    :else (str " (" author-birth " - " author-death ")"))
        return-str (str author-name plate-date )]
        return-str)
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", " "written by " (authors->string (:authors book)))
              )

(defn books->string [books]
  (let [book-nr-str (cond
                    (= (count books) 0) "No books."
                    (= (count books) 1) "1 book."
                    :else (str (count books) " books."))]
    (cond
     (= (count books) 0) (str book-nr-str)
     :else (str book-nr-str " "(apply str (interpose ". " (map book->string books))) ".") )

    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? (old-book->new-book book) author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn[author] (= (:name author) name)) authors)
  )
  )


(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
   (if(= (living-authors (:authors book)) () ) false true ))

(defn books-by-living-authors [books]
  (first (filter has-a-living-author? books)))

; %________%
