(in-package :SQL-TO-S-SQL)

(defclass postmodern-object ()
   ((keyword-argument-pair
    :type list
    :initarg :kw-arg-pair
    :initform nil
    :accessor kw-arg-pair
    :documentation "An a-list for keeping all keyword-argument pairs found in the list-query")
   (entities
    :type list
    :initarg :entities
    :initform nil
    :accessor entities
    :documentation "A list of sublists regarding columns/comparators/sets/tables, their type, and an identifier")
   (query-components
    :type list
    :initarg :query-components
    :initform nil
    :accessor query-components
    :documentation "An a-list for keeping all the parts of the queries, each delimited by a keyword and containing arguments.")
   (dot-components
    :type list
    :initarg :dot-components
    :initform nil
    :accessor dot-components
    :documentation "A list to keep dot components together")
   (comma-components
    :type list
    :initarg :comma-components
    :initform nil
    :accessor comma-components
    :documentation "A list to keep comma components together")
   (target-variable
    :type symbol
    :initarg :target-variable
    :initform nil
    :accessor target-variable
    :documentation "A slot to keep the unbound variable from the predicate network. It is used at the end of the parser"))
  (:documentation "An object representation of an sql query as written in postmodern"))

;------------------------;
;          -1-           ;
; pm-query --> pm-object ;
;------------------------;

(defun make-kw-list (query-list)
  "Makes a list of the keywords of a given list"
  (let ((kw-list '()))
    (loop for item in query-list
          do (when (keywordp item)
               (pushend item kw-list)))
    kw-list))

(defun dissect-pairs (pm-query)
  "For each sublist of a pair in a list, making a new pair out of it"
  (let* ((sql-query (copy-list pm-query))
        (final-pairs '())
        (first-kws (make-kw-list sql-query)))
    (loop while (> (length sql-query) 0)
          for first-elem = (first sql-query)
          do (if (and (keywordp first-elem) (not (eq ':select first-elem)))
               (progn
                 (setf next-kw (get-next-elem first-elem first-kws))
                 (if next-kw 
                   (setf kw-args (in-btween-elems 0 (position next-kw sql-query) sql-query))
                   (setf kw-args (in-btween-elems 0 (length sql-query) sql-query)))
                 (pushend (cons first-elem kw-args) final-pairs)
                 (pop sql-query)
                 (pop first-kws))
             (if (consp first-elem)
               (progn
                 (pushend first-elem final-pairs)
                 (setf new-kws (make-kw-list first-elem))
                 (pop sql-query)
                 (loop for item in first-elem
                       do (when (and (keywordp item) (not (eq ':select item)))
                            (setf next-kw (get-next-elem item new-kws))
                            (pop new-kws)
                            (if next-kw
                              (setf kw-args (in-btween-elems (position item first-elem) (position next-kw first-elem) first-elem))
                              (setf kw-args (in-btween-elems (position item first-elem) (length first-elem) first-elem)))
                            (pushend (cons item kw-args) final-pairs))
                          (when (consp item)
                            (push item sql-query))))
               (pop sql-query))))
    (setf final-pairs (remove-duplicates final-pairs :test 'equal))
    final-pairs))

(defun to-arguments-list (final-pairs-list pm-object)
  "Making lists of the arguments present in the query list."
  (let ((simple-column-predicates '(:group-by :count :round :distinct :avg :max :min :sum :desc))
        (column-comparator-predicates '(:like := :< :> :<= :>= :<> :/ :+))
        (join-predicates '(:left-join :inner-join :right-join :outer-join))
        (columns-list '())
        (alias-list '())
        (comparators-list '())
        (sets-list '())
        (tables-list '()))
    ;select keyword case
    (loop for select-elem in (assoc-all ':select final-pairs-list)
          for possible-elems = (in-btween-elems -1 (position ':from select-elem) select-elem)
          do (loop for elem in possible-elems
                   do (when (symbolp elem)
                        (pushend elem columns-list))))
    ;from keyword case
    (loop for predicate in join-predicates
          for assoc-all = (assoc-all predicate final-pairs-list)
          do (when assoc-all
               (loop for assoc in assoc-all
                   do (loop for elem in assoc
                            do (when (symbolp elem)
                                 (pushend elem tables-list))))))
    (loop for table-elem in (assoc-all ':from final-pairs-list)
          do (loop for elem in table-elem
                   do (when (symbolp elem)
                        (pushend elem tables-list))))
    ;between keyword case
    (loop for between-elem in (assoc-all ':between final-pairs-list)
            do (when (and between-elem (symbolp (first between-elem)))
                 (pushend (first between-elem) columns-list))
               (pushend (second between-elem) comparators-list)
               (pushend (third between-elem) comparators-list))
    ;as keyword case
    (loop for as-elem in (assoc-all ':as final-pairs-list)
          do (when as-elem
               (when (symbolp (first as-elem)) (pushend (first as-elem) tables-list))
               (pushend (second as-elem) alias-list)))
    ;set keyword cases
    (loop for set-elem in (assoc-all ':set final-pairs-list)
          do (when set-elem
               (pushend set-elem sets-list)))
    ;in keyword case
    (loop for in-elem in (assoc-all ':in final-pairs-list)
          do (when in-elem
               (pushend (first in-elem) columns-list)))
    ;order keyword case
    (loop for order-elem in (assoc-all ':order-by final-pairs-list)
          do (when (symbolp (second order-elem))
               (pushend (second order-elem) columns-list)))
    ;limit keyword case
    (loop for limit-elem in (assoc-all ':limit final-pairs-list)
          do (when (integerp (second limit-elem))
               (pushend (second limit-elem) comparators-list)))
    ;other-cases
    (loop for predicate in simple-column-predicates
          for assoc-list = (assoc-all predicate final-pairs-list)
          do (when assoc-list
               (loop for item in assoc-list
                       do
                       (when (and (not (consp (first item))) (not (integerp (first item))))
                         (pushend (first item) columns-list))
                       (when (integerp (first item))
                         (pushend (first item) comparators-list)))))
    (loop for predicate in column-comparator-predicates
          for assoc-list = (assoc-all predicate final-pairs-list)
          do (when assoc-list
               (loop for item in assoc-list
                     for first-elem = (first item)
                     for second-elem = (second item)
                     do (when (symbolp first-elem)
                          (pushend first-elem columns-list))
                        (if (symbolp second-elem)
                          (pushend second-elem columns-list)
                          (when (not (consp second-elem))
                            (pushend second-elem comparators-list))))))
    (loop for column in columns-list
          for column-string = (symbol-name column)
          for join-split = ""
          do (when (find #\. column-string :test #'equal)
               (setf join-split (separate-symbols column))
               (pushend (first join-split) tables-list)
               (pushend (second join-split) columns-list)
               (pushend `(dot ,column ,(first join-split) ,(second join-split)) (dot-components pm-object))))
    (values columns-list comparators-list sets-list tables-list alias-list)))

(defun to-entities (pm-object)
  "Making pre-bindings for each db elem with its type and given variable name."
  (multiple-value-bind (columns-list comparators-list sets-list tables-list alias-list) (to-arguments-list (kw-arg-pair pm-object) pm-object)
    (setf columns-list (remove-duplicates columns-list :test #'equal))
    (setf comparators-list (remove-duplicates comparators-list :test #'equal))
    (setf sets-list (remove-duplicates sets-list :test #'equal))
    (setf alias-list (remove-duplicates alias-list :test #'equal))
    (let ((count 0))
      (when alias-list
        (loop for alias in alias-list
                do (when (find alias tables-list)
                     (setf tables-list (remove alias tables-list)))))
      (when columns-list
        (setf count (+ count 1))
        (loop for column in columns-list
              do (push (list column 'column (generate-symbol "?COLUMN-" count)) (entities pm-object))
                 (setf count (+ count 1))))
      (when comparators-list
        (setf count 0)
        (loop for comparator in comparators-list
                do (pushend (list comparator 'concept (generate-symbol "?COMPARATOR-" count))(entities pm-object))
                   (setf count (+ count 1))))
      (when alias-list
        (setf count 0)
        (loop for alias in alias-list
              do (pushend (list alias 'concept (generate-symbol "?ALIAS-" count))(entities pm-object))
                 (setf count (+ count 1))))
      (when sets-list
        (setf count 0)
        (loop for set in sets-list
              do (pushend (list set 'set (generate-symbol "?SET-VALUES-" count))(entities pm-object))
                 (setf count (+ count 1))))
      (setf tables-list (remove-duplicates tables-list :test #'equal))
      (when tables-list
        (setf count 0)
        (loop for table in tables-list
              do (pushend (list table 'table (generate-symbol "?TABLE-" count))(entities pm-object))
                 (setf count (+ count 1)))))))

(defparameter *filters*
  (list '(:< lower-than) '(:= equals) '(:<> different) '(:+ plus) '(:/ divided-by) '(:> greater-than) '(:<= lower-equals) '(:>= greater-equals) '(:and and) '(:or or) '(:in in) '(:not not) '(:as as) '(:like like) '(:where where) '(:from from) '(:inner-join inner-join) '(:left-join left-join) '(:right-join right-join) '(:full-join full-join) '(:outer-join outer-join) '(:on on) '(:group-by group-by) '(:having having) '(:limit limit) '(:desc desc) '(:order-by order-by) '(:all all)))

(defparameter *aggregators*
  (list '(:distinct distinct) '(:count count) '(:round round) '(:avg average) '(:max max) '(:min min) '(:sum sum) '(:between between)))

(defun to-filter-list (pm-object)
  "Makes a list of all the filters of the query"
  (let ((final-pairs (kw-arg-pair pm-object))
        (filter-elems '())
        (set-elems '())
        (select-elems '())
        (aggregate-elems '())
        (count 0))
    (loop for elem in final-pairs
          do (when (and (assoc (first elem) *filters*))
               (push elem filter-elems))
             (when (eql (first elem) ':set)
               (push elem set-elems))
             (when (eql (first elem) ':select)
               (pushend elem select-elems))
             (when (assoc (first elem) *aggregators*)
               (push elem aggregate-elems)))
    (setf filter-elems (remove-duplicates filter-elems :test #'equal))
    (setf set-elems (remove-duplicates set-elems :test #'equal))
    (setf aggregate-elems (remove-duplicates aggregate-elems :test #'equal))
    (loop for filter in filter-elems
          do (push (list filter (generate-symbol "?FILTER-" count)) (query-components pm-object))
               (setf count (+ count 1)))
    (when set-elems
      (setf count 0)
      (loop for set in set-elems
            do (push (list set (generate-symbol "?SET-CLAUSE-" count)) (query-components pm-object))
               (setf count (+ count 1))))
    (setf count 0)
    (loop for select in select-elems
          do (push (list select (generate-symbol "?RESULT-" count)) (query-components pm-object))
             (setf count (+ count 1)))
    (when aggregate-elems
      (setf count 0)
      (loop for aggregator in aggregate-elems
            do (push (list aggregator (generate-symbol "?AGGREGATOR-" count)) (query-components pm-object))
               (setf count (+ count 1))))))

(defun query->object (pm-query)
  "Takes a postmodern query which has a list form, and returns a postmodern object."
  (let ((pm-object (make-instance 'postmodern-object)))
    (setf (kw-arg-pair pm-object) (dissect-pairs pm-query))
    (push pm-query (kw-arg-pair pm-object)) ;; the first elem of the final-pairs is the query itself to which we will assign a variable
    (to-entities pm-object)
    (to-filter-list pm-object)
    pm-object))

;---------------------------------;
;               -2-               ;
; pm-object --> predicate network ;
;---------------------------------;

(defun integrating-bindings (pm-object)
  "Creates a list of bind predicates with their arguments"
  (let* ((bindings (entities pm-object))
        (bindings-clause '())
        (possible-assoc (list)))
    (loop for binding in bindings
          do (when (and (symbolp (first binding)) (not (find #\. (symbol-name (first binding)) :test #'equal)))
               (push `(bind ,(second binding) ,(third binding) ,(first binding)) bindings-clause))
             (when (not (symbolp (first binding)))
               (push `(bind ,(second binding) ,(third binding) ,(first binding)) bindings-clause)))
    bindings-clause))

(defun integrating-dot-predicates (pm-object)
  "Sends to a list sublists starting with the dot predicate linking an alias/a table to a column"
  (let ((dot-components (dot-components pm-object))
        (dot-predicates '()))
    (loop for dot-component in dot-components
          for dot-predicate = '()
          do (loop for arg in dot-component
                   do (if (assoc arg (entities pm-object))
                        (pushend (third (assoc arg (entities pm-object))) dot-predicate)
                        (pushend arg dot-predicate)))
             (push dot-predicate dot-predicates))
    dot-predicates))


(defun comma-predicate (col-list n-comma pm-object)
  "Creates a comma predicate linking 2 columns when separated by a white space in the query"
  (let ((comma-list '())
        (comma-clause-nb)
        (first-elem)
        (second-elem))
    (setf n-comma (+ n-comma 1))
    (if (consp (first col-list))
      (setf first-elem (second (assoc (first col-list) (query-components pm-object) :test 'equal)))
      (setf first-elem (third (assoc (first col-list) (entities pm-object) :test 'equal))))
    (if (consp (second col-list))
      (setf second-elem (second (assoc (second col-list) (query-components pm-object) :test 'equal)))
      (setf second-elem (third (assoc (second col-list) (entities pm-object) :test 'equal))))
    (setf comma-clause-nb (generate-symbol "?COMMA-CLAUSE-" n-comma))
    (push `(comma ,comma-clause-nb ,first-elem ,second-elem) comma-list)
    (pop col-list)
    (pop col-list)
    (setf n-comma (+ n-comma 1))
    ;Doing the same for the case where there are more columns
    (loop while (> (length col-list) 0)
          do (if (consp (first col-list))
               (setf elem (second (assoc (first col-list) (query-components pm-object) :test 'equal)))
               (setf elem (third (assoc (first col-list) (entities pm-object) :test 'equal))))
             (setf comma-clause-nb (generate-symbol "?COMMA-CLAUSE-" n-comma))
             (push `(comma ,comma-clause-nb ,(second (first comma-list)) ,elem) comma-list)
             (pop col-list)
             (setf n-comma (+ n-comma 1)))
    (loop for comma in comma-list
          do (push comma (comma-components pm-object)))
    comma-list))

(defparameter *filter-args*
  (list '(:< lower-than) '(:= equals) '(:<> different) '(:+ plus) '(:/ divided-by) '(:> greater-than) '(:<= lower-equals)'(:>= greater-equals) '(:and and) '(:or or) '(:in in) '(:not not) '(:distinct distinct) '(:count count) '(:round round) '(:avg average) '(:max max)  '(:min min) '(:sum sum) '(:set set) '(:between between) '(:like like) '(:as as) '(:having having) '(:group-by group-by) '(:select select) '(:from from) '(:where where) '(:inner-join inner-join) '(:left-join left-join) '(:right-join right-join) '(:on on) '(:limit limit) '(:desc desc) '(:order-by order-by) '(:all all)))

(defun merging-variables (pm-object)
  "Looping over the keyword-argument pairs, replaces any argument with its corresponding variable"
  (let ((filters-list '())
        (kw-arg-pairs (kw-arg-pair pm-object)))
    (setf kw-arg-pairs (remove-duplicates kw-arg-pairs))
    (loop for kw-arg-pair in kw-arg-pairs
          for predicate-list = '()
          do (if (eq (first kw-arg-pair) ':select) ;case 1 : it starts with select
               (progn
                 (pushend 'select predicate-list)
                 (pushend (second (assoc kw-arg-pair (query-components pm-object) :test 'equal)) predicate-list)
                 (setf select-elem (in-btween-elems (position ':select kw-arg-pair) (position ':from kw-arg-pair) kw-arg-pair))
                 (if (= (length select-elem) 1)
                   (progn
                     (when (assoc (first select-elem) (entities pm-object) :test 'equal)
                       (pushend (third (assoc (first select-elem) (entities pm-object) :test 'equal)) predicate-list))
                     (when (assoc (first select-elem) (query-components pm-object) :test 'equal)
                       (pushend (second (assoc (first select-elem) (query-components pm-object) :test 'equal)) predicate-list)))
                   (progn
                     (setf comma-list (comma-predicate select-elem (length (comma-components pm-object)) pm-object))
                     (loop for comma-predicate in comma-list
                             do (push comma-predicate filters-list))
                     (pushend (second (first comma-list)) predicate-list)))
                 (pop kw-arg-pair) ;we pop the list so that the "select" kw cannot be taken into account anymore
                 (loop for arg in kw-arg-pair ;we make pairs of kw - args and look for them in (query-components pm-object)
                       do (when (eq arg ':from)
                            (progn
                              (setf kw-list (make-kw-list kw-arg-pair))
                              (if (get-next-elem ':from kw-list)
                                (setf from-elem (in-btween-elems (position ':from kw-arg-pair) (position (get-next-elem ':from kw-list) kw-arg-pair) kw-arg-pair))
                                (setf from-elem (in-btween-elems (position ':from kw-arg-pair) (length kw-arg-pair)  kw-arg-pair)))
                              (if (> (length from-elem) 1)
                                (progn 
                                  (push ':from from-elem)
                                  (setf from-assoc (assoc from-elem (query-components pm-object) :test 'equal))
                                  (pushend (second from-assoc) predicate-list))
                                (pushend (second (assoc (cons arg (list (get-next-elem arg kw-arg-pair))) (query-components pm-object) :test 'equal)) predicate-list))))
                            (when (and (keywordp arg) (not (eq arg ':from)))
                              (pushend (second (assoc (cons arg (list (get-next-elem arg kw-arg-pair))) (query-components pm-object) :test 'equal)) predicate-list))
                          (pop kw-arg-pair))
                 (push predicate-list filters-list))
               (progn
                 (setf comma-list '())
                 (pushend (second (assoc (first kw-arg-pair) *filter-args* :test 'equal)) predicate-list)
                 (pushend (second (assoc kw-arg-pair (query-components pm-object) :test 'equal)) predicate-list)
                 (when (eql (first kw-arg-pair) ':set) ;; exceptional case : set elements
                   (pushend (third (assoc (cdr kw-arg-pair) (entities pm-object) :test 'equal)) predicate-list))
                 (when (eql (first kw-arg-pair) ':from)
                   (setf from-elem (in-btween-elems (position ':from kw-arg-pair) (length kw-arg-pair)  kw-arg-pair))
                   (if (> (length from-elem) 1)
                     (progn
                       (setf comma-list (comma-predicate from-elem (length (comma-components pm-object)) pm-object))
                       (pushend (second (first comma-list)) predicate-list))
                     (progn
                       (when (assoc (second kw-arg-pair) (query-components pm-object) :test 'equal)
                         (pushend (second (assoc (second kw-arg-pair) (query-components pm-object) :test 'equal)) predicate-list))
                       (when (assoc (second kw-arg-pair) (entities pm-object) :test 'equal)
                         (pushend (third (assoc (second kw-arg-pair) (entities pm-object) :test 'equal)) predicate-list)))))
                 (loop for arg in kw-arg-pair ; any other case
                       do (when (not (eql (first kw-arg-pair) ':from))
                            (when (assoc arg (query-components pm-object) :test 'equal)
                              (pushend (second (assoc arg (query-components pm-object) :test 'equal)) predicate-list))
                            (when (assoc arg (entities pm-object) :test 'equal)
                              (pushend (third (assoc arg (entities pm-object) :test 'equal)) predicate-list))))
                 (pushend predicate-list filters-list)
                 (when comma-list
                   (loop for comma-predicate in comma-list
                         do (pushend comma-predicate filters-list))))))
    filters-list))

(defun pm-object->predicates (pm-object)
  "Takes a postmodern object, and returns an sql predicate network"
  (let ((bindings (integrating-bindings pm-object))
        (filters (merging-variables pm-object))
        (dot-predicates (integrating-dot-predicates pm-object))
        (all-predicates '()))
    (loop for binding in bindings
            do (push binding all-predicates))
    (loop for filter in filters
          do (push filter all-predicates))
    (when dot-predicates
      (loop for dot-predicate in dot-predicates
            do (push dot-predicate all-predicates)))
    (setf all-predicates (remove-duplicates all-predicates :test 'equal))
    all-predicates))
           
(defun object->predicate-network (pm-object)
  (let ((predicate-network (pm-object->predicates pm-object)))
    (draw-irl-program predicate-network :open t)))
 
;---------------------------------;
;               -3-               ;
; predicate network --> pm-object ;
;---------------------------------;

(defun get-entities (predicate-network pm-object)
  "Takes a predicate network a sends the entities to the slot Entity of a pm-object"
  (let ((bindings (assoc-all 'bind predicate-network))
        (dot-columns (assoc-all 'dot predicate-network)))
    (loop for binding in bindings
          for binding-type = (first binding)
          for variable = (second binding)
          for entity = (third binding)
          do (if (symbol-starts-with variable "?SET-")
               (push (append (list variable) entity) (entities pm-object))
               (push (list variable entity) (entities pm-object))))
    (loop for column in dot-columns
          for table = (second (assoc (second column) (entities pm-object)))
          for col = (second (assoc (third column) (entities pm-object)))
          for dot-column = '()
          do (progn
               (pushend (first column) dot-column)
               (pushend (generate-symbol-3 table "." col) dot-column)
               (pushend dot-column (entities pm-object))))))

(defparameter *pm-keywords* '((count :count) (average :avg) (equals :=) (plus :+) (divided-by :/) (and :and) (or :or) (in :in) (not :not) (comma :comma) (set :set) (group-by :group-by) (having :having) (greater-than :>) (greater-equals :>=) (lower-than :<) (lower-equals :<=) (distinct :distinct) (between :between) (like :like) (from :from) (where :where) (select :select) (as :as) (max :max) (min :min) (inner-join :inner-join) (left-join :left-join) (on :on) (desc :desc) (sum :sum) (limit :limit) (order-by :order-by) (different :<>) (all :all)))
        
(defun get-components (predicate-network pm-object)
  "Takes a predicate network a sends the components to the relative slot of a pm-object"
  (let ((pre-components '()))
    (loop for item in predicate-network
          for keyword = (first item)
          for value-out = (second item)
          for component = '()
          do (progn
               (when (and (and (not (eql keyword 'bind)) (not (eql keyword 'dot))))
                 (pushend value-out component)
                 (if (eql keyword 'equals)
                   (pushend ':= component)
                   (pushend (second (assoc keyword *pm-keywords*)) component))
                 (loop for arg in (nthcdr 2 item)
                         do (pushend arg component))
                 (pushend component pre-components))))
    ;set is a special case, we manage it before hand:
    (loop for item in pre-components
          for variable = (first item)
          for set-clause = (symbol-starts-with variable "?SET")
          do (when set-clause
               (setf set-value (third item))
               (setf pre-components (remove item pre-components))
               (setf item (remove set-value item))
               (setf item (append item (cdr (assoc set-value (entities pm-object)))))
               (push item pre-components)))
    ;then we manage other cases:
    (loop for item in pre-components
          for value-out = (first item)
          for component = (cdr item)
          do (loop for arg in component
                     for assoc = (assoc arg pre-components)
                     for assoc2 = (assoc arg (entities pm-object))
                     do (when assoc
                          (if (or (eq (first (cdr assoc)) ':from) (eq (first (cdr assoc)) ':where) (eq (first (cdr assoc)) ':inner-join) (eq (first (cdr assoc)) ':left-join) (eq (first (cdr assoc)) ':right-join) (eq (first (cdr assoc)) ':on) (eq (first (cdr assoc)) ':group-by) (eq (first (cdr assoc)) ':having) (eq (first (cdr assoc)) ':comma))
                            (progn
                              (setf pos (position arg component))
                              (remove-element-by-index component pos)
                              (loop for item in (cdr assoc)
                                  do (when (not (eql item ':comma))
                                       (insert-at-position component item pos)
                                       (setf pos (+ pos 1)))))
                              (replace-element component (position arg component) (cdr assoc))))
                        (when assoc2
                          (replace-element component (position arg component) (second assoc2))))
             (push (cons value-out component) (query-components pm-object)))))

(defun predicates->pm-object (predicate-network)
  "Takes a predicate-network and creates a postmodern query"
  (let ((pm-object (make-instance 'postmodern-object)))
    (get-entities predicate-network pm-object)
    (get-components predicate-network pm-object)
    (setf (target-variable pm-object) (get-target-var predicate-network))
    pm-object))

;------------------------;
;           -4-          ;
; pm-object --> pm-query ;
;------------------------;

(defun predicates->pm-query (predicate-network)
  "Takes a predicate-network and creates a lispy postmodern query"
  (let* ((pm-object (predicates->pm-object predicate-network))
         (components (query-components pm-object))
         (pm-query (cdr (assoc (target-variable pm-object) components :test 'equal))))
    pm-query))

(defun test-query (pm-query)
  "Compiles the query in a way that the postmodern library can process it."
  (let* ((compiled-query (sql-compile pm-query))
        (result (query compiled-query)))
    result))

(defun check-query (pm-query)
  "From a pm-query, goes to a predicate network, and back to a pm-query, and tests it"
  (test-query (predicates->pm-query (pm-object->predicates (query->object pm-query)))))