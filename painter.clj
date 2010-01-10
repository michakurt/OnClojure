;(use 'clj-stacktrace.repl)
(use 'clojure.contrib.trace)
(ns painter)


(def *db* (ref {}))

(defn db-push [key l]
  (do
     (dosync
      (alter *db* assoc key (conj (*db* key) l)))))


(defmacro fact [pred & attrs]
  `(do
     (db-push '~pred '(~@attrs))))

(defn db-query [key]
  (*db* key))

(defn is-match [pattern data bindings]
  (let [f (first pattern)
    d (first data)]
    (cond
     (nil? f) true
     (= f '?) (is-match (rest pattern) (rest data) (cons d bindings) )
     true      (and (= f d) (is-match (rest pattern) (rest data) (cons d bindings) )))))

(defn generate-bindings [pattern data]
  (let [f (first pattern)
    d (first data)]
    (cond
     (nil? f) '()
     (= f :?) ,(cons d (generate-bindings (rest pattern) (rest data)))
     (= f d)  ,(cons d (generate-bindings (rest pattern) (rest data)))
     true     ,(cons :none (generate-bindings (rest pattern) (rest data))   ))))
     
(defn varsym? [s]
  (and (symbol? s)
       (= \? (first (name s)))))


(defn bind-if-match-or-nil [k v bds]
  (let [bound-val (k bds)]
    (cond
     (nil? bound-val) (assoc bds k v)
     (= bound-val v)  bds
     true             nil)))


(defn match
  ([pattern data] (match pattern data {}))
  ([pattern data bds]
  ;kein var -> = weiter, sonst stop
  ;     var -> schon gebunden ? = weiter, sonst stop. binden an data
  (let [f (first pattern)
	d (first data)]
    (cond
     (nil? bds)  nil
     (nil? f)    bds
     (varsym? f) (match (rest pattern) (rest data) (bind-if-match-or-nil f d bds))
     (= f d)     (match (rest pattern) (rest data) bds)
     true        nil))))

;(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

;(lookup 'painter '(:?x :?y :?z) '{:?z english})
(defn lookup [pred pattern bds]
  (let [data (*db* pred)]
    ;data durchgehen
    ;matches sammeln
    (filter #(not (nil? %)) (map (fn [x] (match pattern x bds)) data))))

(def interpret-query)

;wenn interpret-query mit diesen bds nicht leer ist, '() zurueckgeben. Sonst bds
(defn interpret-not [clause bds]
  (if  (empty? (interpret-query clause bds)) (list bds)
       '()))
    
(defn interpret-or [clauses bds]
  (mapcat #(interpret-query % bds) clauses))

;interpret-and gibt eine liste von bindings zurueck, dir alle clauses erfüllen. Wendet man darauf einen ausdruck an,
;bleiben die übrig, die außerdem noch diesen ausdruck erfüllen
(defn interpret-and [clauses bds]
  (do
  (if (empty? clauses) (list bds)
      (mapcat #(interpret-query (first clauses) %) (interpret-and (rest clauses) bds)))))

;(interpret-query '(and (painter :?x :?y venetian) (dates :?x 1697 :?w)))
(defn interpret-query
 ([expr] (interpret-query expr {}))
 ([expr bds]
    (let [e (first expr)]
      (cond
       (= e 'and) (interpret-and (reverse (rest expr)) bds)
       (= e 'not) (interpret-not (first (rest expr)) bds)
       (= e 'or)  (interpret-or (rest expr) bds)
       true        (lookup e (rest expr) bds))
      )))


(defn -dynlet [bvec body]
  `(let ~bvec ~body))

(defmacro dynlet [bvec body]
  `(eval (-dynlet ~bvec ~body)))

(defn -dynletmap [bds body]
  `(let ~(vec (mapcat (fn [x] (list x `'~(x bds))) (keys bds))) ~body))

(defmacro dynletmap [bvec body]
  `(eval (-dynletmap ~bvec ~body)))


;usage: (with-answer (painter ?a ?b english) (prn ?a))
(defmacro with-answer [query body]
  `(do
       ~@(map
	(fn [x]
	  (-dynletmap x body)
	  )
	(interpret-query query)))
)


;test
;The first name and nationality of every painter called Hogarth.
(with-answer (painter hogarth ?x ?y)
	     (prn (list ?x ?y)))
;(WILLIAM ENGLISH)
;NIL
;The last name of every painter born in 1697. (Our original example.)
(with-answer (and (painter ?x ?1 ?2)
		  (dates ?x 1697 ?3))
     (prn (list ?x)))
;(CANALE)(HOGARTH)
;NIL
;The last name and year of birth of everyone who died in 1772 or 1792.
(with-answer (or (dates ?x ?y 1772)
		 (dates ?x ?y 1792))
	     (prn (list ?x ?y)))
;(HOGARTH 1697)(REYNOLDS 1723)
;NIL

;The last name of every English painter not born the same year as a Venetian
;one.
(with-answer (and (painter ?x ?1 english)
		  (dates ?x ?b ?2)
		  (not (and (painter ?x2 ?3 venetian)
			    (dates ?x2 ?b ?4))))
	     (prn ?x))
;REYNOLDS
;NIL
