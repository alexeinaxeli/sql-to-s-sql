(ql:quickload :SQL-TO-S-SQL)
(in-package :SQL-TO-S-SQL)

;-----------------------------------;
; SQL query (string) -> S-SQL query ;
;-----------------------------------;

;; tests here are made on the Geoquery sql database
;(disconnect-toplevel)
;(postmodern:connect-toplevel "geography.db" "postgres" "postgres" "localhost")

;;json-file
(defparameter *input-json* (read-json-data "systems/postmodern-parser/data/geography-pm.json"))

(defun writing-sql-data-to-json (input-json output-json)
  "Writing the data in json file"
  (with-open-file (out-stream output-json :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out-stream "\[")
    (loop for block in input-json
          for sql-queries = (cdr (assoc ':sql block))
          do (when (= (length sql-queries) 1)
                 (loop for sentence in (cdr (assoc ':sentences block))
                       for question = (cdr (assoc ':TEXT sentence))
                       for variables = (cdr (third sentence))
                       for sql-query = (first sql-queries)
                       do (if (= (length variables) 0)
                            (progn 
                              (setf s-sql-query (parsing-sql-query sql-query))
                              (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                              (setf answer (check-query s-sql-query))
                              (setf sql-query (cl-ppcre:regex-replace-all "\"" sql-query "\'"))
                              (setf answer-goal (query sql-query))
                              (format out-stream "{\"question\":\"~A\", \"sql-query\":\"~A\", \"s-sql-query\":\"~A\", \"predicate-network\":\"~A\", \"answer\":\"~A\", \"goal-answer\":\"~A\"},~%"  question sql-query s-sql-query pred-network answer answer-goal))
                            (progn
                              (loop for variable in variables
                                    for var-name = (first variable)
                                    for var-val = (cdr variable)
                                    do (setf var-name (string-replace var-name "--" "_"))
                                       (setf var-name (string-replace var-name "-" ""))
                                       (setf question (string-replace question var-name var-val))
                                       (setf sql-query (string-replace sql-query var-name var-val)))
                              (setf s-sql-query (parsing-sql-query sql-query))
                              (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                              (setf answer (check-query s-sql-query))
                              (setf sql-query (cl-ppcre:regex-replace-all "\"" sql-query "\'"))
                              (setf answer-goal (query sql-query))
                              (format out-stream "{\"question\":\"~A\", \"sql-query\":\"~A\", \"s-sql-query\":\"~A\", \"predicate-network\":\"~A\", \"answer\":\"~A\", \"goal-answer\":\"~A\"},~%"  question sql-query s-sql-query pred-network answer answer-goal)))))
             (when (and (> (length sql-queries) 1) (< (length sql-queries) 10))
               (loop for query in sql-queries
                     do (loop for sentence in (cdr (assoc ':sentences block))
                              for question = (cdr (assoc ':TEXT sentence))
                              for variables = (cdr (third sentence))
                              for sql-query = query
                              do (if (= (length variables) 0)
                                     (progn
                                       (setf s-sql-query (parsing-sql-query sql-query))
                                       (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                                       (setf answer (check-query s-sql-query))
                                       (setf sql-query (cl-ppcre:regex-replace-all "\"" sql-query "\'"))
                                       (setf answer-goal  (query sql-query))
                                       (format out-stream "{\"question\":\"~A\", \"sql-query\":\"~A\", \"s-sql-query\":\"~A\", \"predicate-network\":\"~A\", \"answer\":\"~A\", \"goal-answer\":\"~A\"},~%"  question sql-query s-sql-query pred-network answer answer-goal))
                                     (progn
                                       (loop for variable in variables
                                             for var-name = (first variable)
                                             for var-val = (cdr variable)
                                             do (setf var-name (string-replace var-name "--" "_"))
                                                (setf var-name (string-replace var-name "-" ""))
                                                (setf question (string-replace question var-name var-val))
                                                (setf sql-query (string-replace sql-query var-name var-val)))
                                       (setf s-sql-query (parsing-sql-query sql-query))
                                       (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                                       (setf answer (check-query s-sql-query))
                                       (setf sql-query (cl-ppcre:regex-replace-all "\"" sql-query "\'"))
                                       (setf answer-goal  (query sql-query))
                                       (format out-stream "{\"question\":\"~A\", \"sql query\":\"~A\", \"s-sql-query\":\"~A\", \"predicate-network\":\"~A\", \"answer\":\"~A\", \"goal-answer\":\"~A\"},~%"  question sql-query s-sql-query pred-network answer answer-goal)))))))
    (format out-stream "\]")))

(defparameter *output-json* "systems/postmodern-parser/data/geoquery-for-pm.json")
(writing-sql-data-to-json *input-json* *output-json*)


;; checking that all answers are correct

(defparameter *input-json2* (read-json-data "systems/postmodern-parser/data/geoquery-for-pm.json"))

(defun checking-answer (input-json)
  "Checking that answer retrieved by the s-sql query after being parsed is equal to sql query answer"
  (loop for block in input-json
        for answer = (assoc ':answer block)
        for goal-answer = (assoc ':goal-answer block)
        do (when (not (string= (cdr answer) (cdr goal-answer)))
            (print block))))

(checking-answer *input-json2*)


;; writing all the data in a jsonl file for pattern-finding experiments

(defun writing-sql-data-to-list (input-json)
  "Writing the data in a list for pattern finding experiments"
  (let ((data-list '())) 
    (loop for block in input-json
          for sql-queries = (cdr (assoc ':sql block))
          do (when (= (length sql-queries) 1)
               (loop for sentence in (cdr (assoc ':sentences block))
                     for question = (cdr (assoc ':TEXT sentence))
                     for variables = (cdr (third sentence))
                     for sql-query = (first sql-queries)
                     do (if (= (length variables) 0)
                          (progn
                            (setf s-sql-query (parsing-sql-query sql-query))
                            (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                            (push (list (format nil "{\"utterance\":\"~A\", \"meaning\":\"~A\", \"len\": ~A},~%"  question pred-network (length pred-network)) (length pred-network)) data-list))
                          (progn
                            (loop for variable in variables
                                  for var-name = (first variable)
                                  for var-val = (cdr variable)
                                  do (setf var-name (string-replace var-name "--" "_"))
                                     (setf var-name (string-replace var-name "-" ""))
                                     (setf question (string-replace question var-name var-val))
                                     (setf sql-query (string-replace sql-query var-name var-val)))
                              (setf s-sql-query (parsing-sql-query sql-query))
                              (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                              (push (list (format nil "{\"utterance\":\"~A\", \"meaning\":\"~A\", \"len\": ~A},~%"  question pred-network (length pred-network)) (length pred-network)) data-list)))))
             (when (and (> (length sql-queries) 1) (< (length sql-queries) 10))
               (loop for query in sql-queries
                     do (loop for sentence in (cdr (assoc ':sentences block))
                              for question = (cdr (assoc ':TEXT sentence))
                              for variables = (cdr (third sentence))
                              for sql-query = query
                              do (if (= (length variables) 0)
                                   (progn
                                     (setf s-sql-query (parsing-sql-query sql-query))
                                     (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                                     (push (list (format nil "{\"utterance\":\"~A\", \"meaning\":\"~A\", \"len\": ~A},~%"  question pred-network (length pred-network)) (length pred-network)) data-list))
                                   (progn
                                     (loop for variable in variables
                                           for var-name = (first variable)
                                           for var-val = (cdr variable)
                                           do (setf var-name (string-replace var-name "--" "_"))
                                              (setf var-name (string-replace var-name "-" ""))
                                              (setf question (string-replace question var-name var-val))
                                              (setf sql-query (string-replace sql-query var-name var-val)))
                                     (setf s-sql-query (parsing-sql-query sql-query))
                                     (setf pred-network (pm-object->predicates (query->object s-sql-query)))
                                     (push (list (format nil "{\"utterance\":\"~A\", \"meaning\":\"~A\", \"len\": ~A},~%"  question pred-network (length pred-network)) (length pred-network)) data-list)))))))
    data-list))

(defun sort-and-write-to-jsonl (input-json output-json)
  "Using the data generated by the function writing-sql-data-to-json"
  (let* ((list-data (writing-sql-data-to-list input-json))
         (sorted-list (sort list-data #'< :key 'second)))
    (with-open-file (out-stream output-json :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for block in sorted-list
            do (format out-stream (car block))))))


(defparameter *output-json* "systems/postmodern-parser/data/geography-for-pf.jsonl")
(sort-and-write-to-jsonl *input-json* *output-json*)

;---------------------------------------------;
; S-SQL -> Predicate-network -> S-SQL process ;
;---------------------------------------------;

;; In this section, we use the parser to test queries that have already been written in s-sql format.
;; What we mean by process :
;; 1) taking an s-sql query,
;; 2) transforming it into a predicate network,
;; 3) back to a query,
;; 3) executing the query

;some query examples in s-sql format (used for some examples like the one below)
(defvar *ex-1* '(:select actor :from actorsfilms))
(defvar *ex-2* '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101)))
(defvar *ex-3* '(:select (:count actor) :from actorsfilms))
(defvar *ex-4* '(:select actor film :from actorsfilms))
(Defvar *ex-5* '(:select actor film year :from actorsfilms))
(defvar *ex-6* '(:select (:count actor) (:avg year) :from actorsfilms))
(defvar *ex-7* '(:select film :from actorsfilms :where (:= actor "Gerard Depardieu")))
(defvar *ex-8* '(:select film :from actorsfilms :where (:in actor (:set "Gerard Depardieu" "Fred Astaire"))))
(defvar *ex-9* '(:select film :from actorsfilms :where (:not (:in actor (:set "Gerard Depardieu" "Fred Astaire")))))
(defvar *ex-10* '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100)))
(defvar *ex-11* '(:select (:distinct film) :from films :inner-join years :on (:= films.film_id years.film_id) :where (:= years.year 2021)))
(defvar *ex-12* '(:select (:count film) :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:= actors.actor "Gerard Depardieu")))
(defvar *ex-13*'(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:or (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) (:not (:like actors.actor "Brigitte"))) :group-by films.film :having (:> (:avg films.rating) 9)))
(defvar *ex-14* '(:select (:count film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:between years.year 1915 1916)))
(defvar *ex-15* '(:SELECT (:COUNT (:DISTINCT FILMS.FILM)) :FROM FILMS :INNER-JOIN YEARS :ON (:= FILMS.ACTOR_ID YEARS.ACTOR_ID) :WHERE (:= FILMS.ACTOR "Gerard Depardieu")))

;; Three databases can be tested (it's necessary to disconnect before connecting to another database : 
;; (disconnect-toplevel)

;; (connect-toplevel "db2_actors_films_simple_table.db" "postgres" "postgres" "localhost")
;; (connect-toplevel "db1_films_years.db" "postgres" "postgres" "localhost")
;; (connect-toplevel "db3_actors_films_multiple_tables.db" "postgres" "postgres" "localhost")


;; the following line can be used to test one query in the entire process, we only need to change the name of the example

;(disconnect-toplevel)
;(postmodern:connect-toplevel "geography.db" "postgres" "postgres" "localhost")
(predicates->pm-query (pm-object->predicates (query->object  '(:SELECT (:COUNT HIGHLOWALIAS0.STATE_NAME) :FROM (:AS HIGHLOW HIGHLOWALIAS0) :WHERE (:< HIGHLOWALIAS0.LOWEST_ELEVATION (:SELECT HIGHLOWALIAS1.LOWEST_ELEVATION :FROM (:AS HIGHLOW HIGHLOWALIAS1) :WHERE (:= HIGHLOWALIAS1.STATE_NAME "alabama")))))))

(test-query (predicates->pm-query (pm-object->predicates (query->object  '(:SELECT (:COUNT HIGHLOWALIAS0.STATE_NAME) :FROM (:AS HIGHLOW HIGHLOWALIAS0) :WHERE (:< HIGHLOWALIAS0.LOWEST_ELEVATION (:SELECT HIGHLOWALIAS1.LOWEST_ELEVATION :FROM (:AS HIGHLOW HIGHLOWALIAS1) :WHERE (:= HIGHLOWALIAS1.STATE_NAME "alabama"))))))))

;; (disconnect-toplevel)
;; (connect-toplevel "db2_actors_films_simple_table.db" "postgres" "postgres" "localhost")

(test-query (predicates->pm-query (pm-object->predicates (query->object '(:select film :from actorsfilms :where (:not (:in actor (:set "Gerard Depardieu" "Fred Astaire"))))))))


;; running automatically multiple queries
(defun multiple-tests ()
  (let ((db2_list (list '(:select actor :from actorsfilms)
                        '(:select (:count actor) :from actorsfilms)
                        '(:select actor film :from actorsfilms)
                        '(:select actor film year :from actorsfilms)
                        '(:select (:count actor) (:avg year) :from actorsfilms)
                        '(:select film :from actorsfilms :where (:= actor "Gerard Depardieu"))
                        '(:select film :from actorsfilms :where (:in actor (:set "Gerard Depardieu" "Fred Astaire")))
                        '(:select film :from actorsfilms :where (:not (:in actor (:set "Gerard Depardieu" "Fred Astaire"))))
                        '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100))
                        '(:select (:count film) :from actorsfilms :where (:= actor "Gerard Depardieu"))
                        '(:select actor film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 2.5)))
                        '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101))
                        '(:select actor_id :from actorsfilms :where (:in actor (:set "Gerard Depardieu" "Fred Astaire" "Brigitte Bardot")))
                        '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101))
                        '(:select film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 5)))))
        (db1_list (list '(:select film :from films :inner-join years :on (:= films.film_id years.film_id) :where (:= years.year 2021))
                        '(:select (:distinct film) :from films :inner-join years :on (:= films.film_id years.film_id) :where (:= years.year 2021))
                        '(:select (:count film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:between years.year 1915 1916))
                        '(:select (:count film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:between years.year 1915 1920) (:and (:not (:= years.year 1917)) (:not (:= years.year 1918)))))
                        '(:select (:distinct films.film) years.year :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:= films.actor "Gerard Depardieu"))
                        '(:select (:count (:distinct films.film)) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:= films.actor "Gerard Depardieu"))
                        '(:select film :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:or (:= films.actor "Gerard Depardieu") (:= films.actor "Brigitte Bardot")) (:= years.year "1999")))
                        '(:select (:distinct film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:and (:= films.actor "Gerard Depardieu") (:= years.year 1999)) (:like films.film "The%")))))
        (db3_list (list '(:select (:count film) :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:= actors.actor "Gerard Depardieu"))
                        '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:= actors.actor "Gerard Depardieu") (:= films.year 1996)))
                        '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:like actors.actor "Jean%") (:> films.year 2020)))
                        '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:or (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) (:not (:like actors.actor "Brigitte"))) :group-by films.film :having (:> (:avg films.rating) 9)))))

    (disconnect-toplevel)
    (connect-toplevel "db2_actors_films_simple_table.db" "postgres" "postgres" "localhost")
    (loop for pm-query in db2_list
          do (handler-case (check-query pm-query)
               (error (e)
                 (format t "Error in response from the following query: ~S.~&" pm-query e))))

    (disconnect-toplevel)
    (connect-toplevel "db1_films_years.db" "postgres" "postgres" "localhost")
    (loop for pm-query in db1_list
          do (handler-case (check-query pm-query)
               (error (e)
                 (format t "Error in response from the following query: ~S.~&" pm-query e))))

    (disconnect-toplevel)
    (connect-toplevel "db3_actors_films_multiple_tables.db" "postgres" "postgres" "localhost")
    (loop for pm-query in db3_list
          do (handler-case (check-query pm-query)
               (error (e)
                 (format t "Error in response from the following query: ~S.~&" pm-query e))))))
    
(multiple-tests)

;-------------------------------------------;
;Part 1 Tests: pm-query -> predicate network;
;-------------------------------------------;

;some tests of the query->object function (the name of the example needs to be changed) :
(kw-arg-pair (query->object *ex-5*))
(entities (query->object *ex-5*))
(query-components (query->object *ex-5*))
(dot-components (query->object *ex-5*))

;some use of the functions getting to the predicate network
(pm-object->predicates (query->object *ex-5*))

;-------------------------------------------;
;Part 2 Tests: predicate-network -> pm-query;
;-------------------------------------------;

;(disconnect-toplevel)
;(connect-toplevel "db2_actors_films_simple_table.db" "postgres" "postgres" "localhost")

;; '(:select actor :from actorsfilms)
(defparameter *first-test*
  '((bind column ?select-clause actor)
  (select ?result ?select-clause ?from-clause)
  (from ?from-clause ?table-1)
  (bind table ?table-1 actorsfilms)))
;(query-components (predicates->pm-object *first-test*))
;(entities (predicates->pm-object *first-test*))
;(test-query (predicates->pm-query *first-test*))
