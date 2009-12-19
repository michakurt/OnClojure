;(use 'clj-stacktrace.repl)

(ns painter
  (:require [clojure.walk :as walk]))


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
(defn interpret-not [clauses bds]
  (if
      (empty? (interpret-query clauses bds)) bds
      '()))
    


(defn interpret-or [clauses bds]
  (mapcat #(interpret-query % bds) clauses))

;interpret-and gibt eine liste von bindings zurueck, dir alle clauses erfüllen. Wendet man darauf einen ausdruck an,
;bleiben die übrig, die außerdem noch diesen ausdruck erfüllen
(defn interpret-and [clauses bds]
  (if (empty? clauses) (list bds)
      (mapcat #(interpret-query (first clauses) %) (interpret-and (rest clauses) bds))))

;(interpret-query '(and (painter :?x :?y venetian) (dates :?x 1697 :?w)))
(defn interpret-query
 ([expr] (interpret-query expr {}))
 ([expr bds]
    (let [e (first expr)]
      (cond
       (= e 'and) (interpret-and (rest expr) bds)
       (= e 'not) (interpret-not (rest expr) bds)
       (= e 'or)  (interpret-or (rest expr) bds)
       true        (lookup e (rest expr) bds))
      )))


;usage: (with-answer (painter ?a ?b english) (prn '?a))
(defmacro with-answer [query body]
  `(do
       ~@(map
	(fn [x]
	  (let [rbody# (walk/postwalk-replace x body)]
	    rbody#)
	  )
	(interpret-query query)))
)

;dynamic let with macros

(defmacro dynlet [bvec body]
    `(let ~bvec
       ~body))
; used by pm-progv macro to create an expression of the form
;
; (let (<binding-list)
;    <expression>)
;
; where <binding-list> is of the form (<variable1> <value1>) (<variable2> <value2>) ...
;       <expression> is a Scheme expression
;(define (-pm-progv variables values expression)
;  (let ((l (map (lambda (a b) (list a b))
;                variables
;                values)))
;    (list 'let l expression)));


; simple *similar* version of CL's progv
;(define-macro (pm-progv variables values expression)
;  `(eval (-pm-progv ,variables ,values ,expression) (interaction-environment)))


; used by pm-progv macro to create an expression of the form
;
; (let (<binding-list)
;    <expression>)
;
; where <binding-list> is of the form (<variable1> <value1>) (<variable2> <value2>) ...
;       <expression> is a Scheme expression
(defn -pm-progv [variables values expression]
  (let [l (map (fn [a b] (list a b))
                variables
                values)]
    (list 'let (vec l) expression)))


; simple *similar* version of CL's progv
;(define-macro (pm-progv variables values expression)
;  `(eval (-pm-progv ,variables ,values ,expression) (interaction-environment)))


(defn -dynlet [bvec body]
  `(let ~bvec ~body))

(defmacro dynlet [bvec body]
  `(eval (-dynlet ~bvec ~body)))

(defn -dynletmap [bds body]
  `(let ~(vec (mapcat (fn [x] (list x `'~(x bds))) (keys bds))) ~body))

(defmacro dynletmap [bvec body]
  `(eval (-dynletmap ~bvec ~body)))

(defmacro with-answer [query body]
  `(do
       ~@(map
	(fn [x]
	  (-dynletmap x body)
	  )
	(interpret-query query)))
)
