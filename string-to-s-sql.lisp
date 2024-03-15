(in-package :SQL-TO-S-SQL)

;---------------;
; Preprocessing ;
;---------------;

(defun generate-symbol (symbol-prefix symbol-sufix)
  "Generates a symbol name."
  (let* ((string-symbol (format nil "~a~d" symbol-prefix symbol-sufix))
         (symbol (intern string-symbol)))
    symbol))

(defun separate-parentheses (q)
  "Searches for parenthesis in a string and when it is glued to a character whether on the left or on the right, adds a space to separate it"
  (with-output-to-string (output)
    (dotimes (i (length q))
      (let ((char (char q i)))
        (cond
          ((char= char #\()
           (when (and (plusp i) (not (char= (char q (1- i)) #\Space)))
             (write-char #\Space output))
           (write-char char output)
           (when (and (< (+ i 2) (length q))
                      (not (char= (char q (+ i 1)) #\Space)))
             (write-char #\Space output)))
          ((char= char #\))
           (when (and (> i 0) (not (char= (char q (1- i)) #\Space)))
             (write-char #\Space output))
           (write-char char output)
           (when (and (< i (1- (length q)))
                      (not (char= (char q (+ i 1)) #\Space)))
             (write-char #\Space output)))
          (t
           (write-char char output)))))))

(defun add-space-at-beginning-and-end (q)
  (concatenate 'string " " q " "))

(defun escape-parentheses (string)
  "Escapes parentheses in the given string."
  (with-output-to-string (out)
    (loop for char across string
          do (if (or (char= char #\()
                     (char= char #\)))
                 (write-char #\\ out))
          do (write-char char out))))

(defparameter *sql-keywords* (list (cons "select" ":select") (cons "count" ":count") (cons "avg" ":avg")  (cons "and" ":and") (cons "or" ":or") (cons "not in" ":notin") (cons "in" ":in") (cons "not" ":not") (cons "group by" ":group-by") (cons "having" ":having")  (cons "distinct" ":distinct") (cons "between" ":between") (cons "like" ":like") (cons "from" ":from") (cons "where" ":where")  (cons "as" ":as") (cons "max" ":max") (cons "min" ":min") (cons "inner join" ":inner-join") (cons "left join" ":left-join") (cons "left outer join" ":left-join") (cons "on" ":on") (cons "desc" ":desc") (cons "asc" ":asc") (cons "sum" ":sum") (cons "limit" ":limit") (cons "order by" ":order-by")  (cons "all" ":all")))


(defparameter *sql-operators* (list (cons "+" ":+") (cons "/" ":/") (cons "<>" ":<>") (cons ">=" ":>=") (cons "<=" ":<=")))

(defun replace-kw (q)
  "Takes an sql query string and replaces all keywords with their equivalent in postmodern library"
  (loop for sql-kw in *sql-keywords*
      do (when (search (first sql-kw) q :test #'string-equal)
           (setf q (cl-ppcre:regex-replace-all (format nil "\\b(?i)~a\\b" (first sql-kw)) q (format nil "~a" (cdr sql-kw))))))
  (loop for sql-kw in *sql-operators*
      do (when (search (first sql-kw) q :test #'string-equal)
           (setf q (cl-ppcre:regex-replace-all (format nil "~a" (first sql-kw)) q (format nil "~a" (cdr sql-kw))))))
  (loop for exception in (list (cons ">" ":>") (cons "<" ":<") (cons "=" ":="))
        do (when (search (first exception) q :test #'string-equal)
             (setf q (cl-ppcre:regex-replace-all (format nil "[^:><]~a" (first exception)) q (format nil " ~a" (cdr exception))))))
  q)

(defun replace-commas (q)
  (setf q (cl-ppcre:regex-replace-all #\, q ":comma"))
  (setf q (cl-ppcre:regex-replace-all ";" q ""))
  (setf q (cl-ppcre:regex-replace-all "  " q " "))
  q)

(defun string-to-list (str)
  "From a string returns a list"
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

(defun preprocess-string (q)
  "Takes an sql query string, preprocesses it and returns a list of all its elements"
  (let ((query-elems '()))
    ;; 1st step : Adding spaces around parenthesis when there are none, and a space at the beginning and end of the query + removing commas
    (setf q (separate-parentheses q))
    (setf q (add-space-at-beginning-and-end q))
    (setf q (replace-commas q))
    ;; 2nd step : Replacing keywords by their postmodern library equivalent keyword and treating the "not in" case 
    (setf q (replace-kw q))
    ;; 3rd step : Turning the query into a list of its elements
    (setf query-elems (string-to-list q))
    query-elems))

;---------;
; Parsing ;
;---------;

(defun list-lexer (q)
  "Takes as argument a list made out of an sql query and returns a list of pairs lexem's type - lexem"
  #'(lambda ()
      (let ((value (pop q)))
        (if (null value) (values nil nil)
            (let ((terminal
                   (cond ((member value '(:+ :- :* :/ := :> :< :>= :<= :<> :as)) 'binary-pred) ;; binary predicates
                         ((member value '(:distinct :all)) 'misc)
                         ((eq value ':between) 'between) 
                         ((eq value ':select) 'select)
                         ((eq value ':comma) 'comma)
                         ((eq value ':not) 'not) ;it is also an unary predicate but it works slightly differently as it can negates a binary predicate and thus it needs to be treated after
                         ((eq value ':notin) 'notin)
                         ((member value '(:from :where :inner-join :left-join :on :group-by :having)) 'fixed-pred) ;; predicates that don't interact in a certain way, they don't move
                         
                         ((eq value ':in) 'in)
                         ((member value '(:and :or)) 'logical-op) ;; logical operators
                         ((member value '(:count :avg :max :min :sum)) 'unary-pred) ;; unary predicates
                         ((member value '(:asc :desc)) 'order-pred) ;; predicates that define the order of the result
                         ((eq value ':order-by) 'order-by)
                         ((eq value ':limit) 'limit)
                         ((and (consp value) (eq (first value) ':select)) 'subquery) 
                         (t 'other))))
              ;(format t "Terminal: ~S, Value: ~S~%" terminal value)
              (values terminal value))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c) 
    "Infix to prefix"
    (when (and (eq b ':in) (consp c) (not (keywordp (first c))))
      (push ':SET c))
    (list b a c))
  
  (defun k-2-3 (a b c)
    "Keeping b, a and c are removed"
    (declare (ignore a c))
    b)

  (defun lst-of-2 (b c)
    "Making a list out of b and c"
    (list b c))

  (defun inv-lst-of-2 (a b)
    "Making an list out of b and a, in this order"
    (list b a))
  
  (defun adding-successors (a b c)
    "Adding b to a already parsed, and then c to the same list"
      (if (and (eq b ':from) (consp c) (not (eq (first c) ':as)))
        (progn 
          (pushend b a)
          (append a c))
        (progn
          (pushend b a)
          (pushend c a))))
  
  (defun a-3 (a b c)
    "Adding c to a without b"
    (if (and (consp a) (not (eq (first a) ':as)))
      (pushend c a)
      (list a c)))
  
  (defun push-2-lst (b c)
    "Sets b in first place of the list c"
    (if (and (consp c) (or (eq (first c) ':distinct) (eq (first c) ':in)))
        (setf list-of-lists (list b c))
        (push b c)))

  (defun not-in-case (a b c)
   (list ':NOT (list ':IN a c)))

  (defun b2in (a b c)
    "Takes care of the between kw case"
    (list b a (second c) (third c)))
  
  (defun subquery-processing (a)
    (parse-with-lexer (list-lexer a) *expression-parser*)))

(define-parser *expression-parser*
  (:start-symbol expression)
  (:terminals (select misc between unary-pred notin not subquery comma binary-pred in logical-op other fixed-pred |(| |)| order-pred order-by limit))
  (:precedence ((:left subquery) (:left misc) (:left unary-pred) (:left binary-pred) (:left notin) (:left in) (:left not) (:left logical-op) (:left between) (:left select) (:left comma) (:left fixed-pred) (:left order-pred) (:left order-by) (:left limit)))
  (expression
   (select expression)
   (subquery #'subquery-processing)
   (expression between expression #'b2in)
   (expression fixed-pred expression #'adding-successors)
   (expression comma expression #'a-3)
   (expression order-pred #'inv-lst-of-2)
   (expression order-by expression #'i2p)
   (expression limit expression #'i2p)
   (misc expression #'lst-of-2)
   (unary-pred expression #'push-2-lst)
   (expression binary-pred expression #'i2p)
   (not expression #'push-2-lst)
   (expression in expression #'i2p)
   (expression notin expression #'not-in-case)
   (expression logical-op expression #'i2p)
   term)
  (term
   other
   nil))

(defun parsing-sql-query (sql-query)
  "The main function, parsing an sql query (being a string) to an s-sql query"
  (let* ((preprocessed-sql (preprocess-string sql-query))
         (s-sql-query (parse-with-lexer (list-lexer preprocessed-sql) *expression-parser*)))
    s-sql-query))

;(parsing-sql-query *ex1*)
;(preprocess-string *ex1*)