(in-package :SQL-TO-S-SQL)

;------------------;
;working with lists;
;------------------;

(defun get-next-elem (elem lst)
  "From a list, sends back an element following the one given in the parameters"
  (let* ((elem-position (position elem lst))
        (next-position (+ elem-position 1))
        (next-elem (nth next-position lst)))
    next-elem))

(defun in-btween-elems (pos1 pos2 query-list)
  "Given two position numbers in a list, retrieves the element strictly between these two positions"
  (let ((list-items '())) ;the items corresponding to the positions specified in the loop below
    (loop for i from (+ pos1 1) to (- pos2 1)
          do (pushend (nth i query-list) list-items))
    list-items))

(defun assoc-all (item alist &key (test #'eql))
  "Return a list of all values associated with ITEM in ALIST."
  (loop for (key . value) in alist
        when (funcall test item key)
        collect value))

(defun remove-element-by-index (lst index)
  (if (zerop index)
      (cdr lst)  ; If index is 0, remove the first element
      (setf (cdr (nthcdr (1- index) lst)) (cddr (nthcdr (1- index) lst))))
  lst)

(defun remove-equal (item lst)
  (remove item lst :test #'equal))

(defun replace-element (lst index new-element)
  (setf (nth index lst) new-element)
  lst)

(defun sublist-first-element-p (element list)
  (let ((elem (find element list :test #'(lambda (x y)
                               (equal element (car y))))))
    elem))

(defun insert-at-position (list element position)
  (let* ((tail (nthcdr (1- position) list))
         (rest (cdr tail)))
    (setf (cdr tail) (cons element rest)))
  list)

;--------------------;
;working with symbols;
;--------------------;

(defun generate-symbol (symbol-prefix symbol-sufix)
  "Generates a symbol name."
  (let* ((string-symbol (format nil "~a~d" symbol-prefix symbol-sufix))
         (symbol (intern string-symbol)))
    symbol))

(defun generate-symbol-3 (symbol-prefix mid-symbol symbol-suffix)
  "Generates a symbol name with 3 components."
  (let* ((string-symbol (format nil "~a~d~a" symbol-prefix mid-symbol symbol-suffix))
         (symbol (intern string-symbol)))
    symbol))

(defun symbol-starts-with (symbol prefix)
  "Checking if a symbol starts a certain way"
  (let ((symbol-string (string symbol)))
    (string= (subseq symbol-string 0 (length prefix)) prefix)))

(defun separate-symbols (symbol)
  (let* ((str (symbol-name symbol))
         (parts (uiop:split-string str :separator "."))
         (symbols-list '()))
    (loop for part in parts
          do (pushend (read-from-string part) symbols-list))
    symbols-list))

;------------;
;json parsing;
;------------;

(defun read-json-data (json-file)
  "function to read data from json file"
  (let* ((file-stream (open json-file :if-does-not-exist nil))
         (json-data (when file-stream (json:decode-json file-stream))))
    (close file-stream)
    json-data))

;-----------;
;csv parsing;
;-----------;

(defun read-csv-file (file-path)
  "Reads a CSV file and returns the data as a list of lists."
  (with-open-file (stream file-path :direction :input)
    (loop
      :for line = (read-line stream nil)
      :while line
      :collect (split-sequence #\; line))))

(defun list-of-sublists-to-csv (list-of-sublists file-path)
  "Write a list of sublists to a CSV file."
  (with-open-file (stream file-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (loop for sublist in list-of-sublists
          do (format stream "~{~A~^,~}~%" sublist))))