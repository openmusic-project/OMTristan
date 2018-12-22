(in-package :om)
;----------------------------------------------------------------------
;----------------------Primitives Le lisp removed from Om--------------
;----------------------------------------------------------------------
(defmacro rassq (item list) `(rassoc ,item ,list :test #'eq))
;;;(defmacro cassq (item list) `(cdr (assq ,item ,list)))
(defmacro newl (lst elem) `(push ,elem ,lst))

(defmacro nextl (lst &optional symb)
  (if symb
    `(setq ,symb (pop ,lst))
    `(pop ,lst) ))

(defmacro vref (vect index) `(svref ,vect ,index))
(defmacro vset (vect index val) `(setf (svref ,vect ,index) ,val))

;; =============================================================================-======

(defmacro tell (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapc #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapc ,fun ,outlet))))

(defmacro ask (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-"))
        (out-var (gensym "OUT-")) (result-var (gensym "RESULT-")))
    `(let ((,args-var (list ,@args)) (,fun-var ,fun) (,result-var nil))
       (dolist (,out-var ,outlet)
         (when (setq ,result-var (apply ,fun-var ,out-var ,args-var))
           (return)))
       ,result-var)))

(defmacro ask-all (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapcar #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapcar ,fun ,outlet))))

;; =============================================================================-======
;; The syntax is different from the Le_Lisp "with"

(defmacro with (l-place-value &body body)
  "Changes locally the value of \"setf-able\" places (like a \"let\" where places
would not be restricted to variables)."
  (let ((places (mapcar #'first l-place-value))
        (values (mapcar #'second l-place-value))
        (vars (mapcar #'(lambda (pv) (declare (ignore pv)) (gensym "WITH-"))
                      l-place-value)))
    `(let ,(mapcar #'list vars places)
       (unwind-protect
         (progn
           ,.(mapcar #'(lambda (place value) `(setf ,place ,value)) places values)
           ,.body)
         ,.(mapcar #'(lambda (place var) `(setf ,place ,var)) places vars)))))

;(let ((l '(a . b))) (with (((car l) 1) ((cdr l) 2)) (print l)))

;; =============================================================================-======
;;**********************************************************************************************************

;; root class for spdata and spdata-seq
(defclass C-spd ()
  ((typ :initform () :initarg :typ :accessor typ) ; typ is addsyn, modres, iana
   (file :initform () :initarg :file :accessor file)
   (last-loaded-file :initform "root:" :accessor last-loaded-file :allocation :class)
   (spd-debug :initform () :accessor spd-debug :allocation :class)
   ))



(defclass* C-spdata (C-spd)
  ((size  :initform 0 :initarg :size :accessor size)
   (frame  :initform 0 :initarg :frame :accessor frame)
   (partials  :initform () :initarg :partials :accessor partials)
   (freqs  :initform () :initarg :freqs :accessor freqs)
   (amps  :initform () :initarg :amps :accessor amps)
   (normalized-amps :initform () :accessor normalized-amps)
   (bws  :initform () :initarg :bws :accessor bws)
   (phases  :initform () :initarg :phases :accessor phases)
   (weights :initform () :initarg :weights :accessor weights)
   (resfact :initform () :accessor resfact)
   (patch :initform () :accessor patch) ; max patch modules; used by write-msgbox
   )
  (:icon 139)
  (:documentation "erer"))


(defmethod copy-spdata ((self C-spdata))
  (make-instance 'C-spdata :size (size self) :frame (frame self)
                                    :partials (partials self) :freqs (freqs self) :amps (amps self) 
                                    :bws (bws self) :phases (phases self) :weights (weights self)
                                    :typ (typ self) :file (file self))
)

(defclass* C-spdata-seq (C-spd)
  ((duration  :initform 0 :initarg :duration :accessor duration)
   (spdata  :initform () :initarg :spdata :accessor spdata) ;; list of time-tagged spectra
   )
  (:icon 139)
  (:documentation "erer"))




;from PW's kernel code
(defmacro until (condition &body body)
  `(while (not ,condition) ,.body))


(defvar *lastspfile* "root")

;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxwhats neededxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; =============================================================================-======

(defun synonym (new-fun old-fun)
  "make symbol <new-fun> a synonym function of symbol <old-fun>"
  (setf (symbol-function new-fun) (symbol-function old-fun))
  (setf (documentation new-fun 'function) (documentation old-fun 'function))
  new-fun )

(defun vector-to-list (vector)
  "Takes the elements of a linear vector and collects them into a list."
  (do ((list () (cons (aref vector index) list))
       (index (1- (length vector)) (1- index)))
      ((< index 0) list)))

#|
(defun vector-to-list1 (vector &aux (length (length vector)) (list ()))
  "Takes the elements of a simple vector and collects them into a list."
  (dotimes (index length)
    (push (svref vector (decf length)) list))
  list )

(time (dotimes (i 10000) (vector-to-list #(a b c d))))
(dotimes (i 10000) (vector-to-list #(a b c d))) took 205 ticks (3.417 seconds) to run.

(time (dotimes (i 10000) (vector-to-list1 #(a b c d))))
(dotimes (i 10000) (vector-to-list1 #(a b c d))) took 218 ticks (3.633 seconds) to run.
|#
;; =============================================================================-======

(defun compile-file? (file)
  "Compiles <file> only if needed."
  (let* ((infile (merge-pathnames file "*.Lisp"))
         (outfile (compile-file-pathname infile)))
    (unless (and (probe-file outfile)
                 (> (file-write-date outfile) (file-write-date infile)))
      (compile-file infile :output-file outfile :verbose t) )))

;; =============================================================================-======

(defun file-compareE ()
  (let*
    ((file1 (oa::om-choose-file-dialog :button-string "Read 1st"))
     (file2 (oa::om-choose-file-dialog :button-string "Read 2nd"))
     (diffname (format () "~A/~A" (pathname-name file1) (pathname-name file2)))
     (outfile
      (oa::om-choose-new-file-dialog
       :directory
       (merge-pathnames (make-pathname  :name diffname :type "diff") file2)
       :prompt "Save difference as:"
       :button-string "Save diff" )))
    (file-compare file1 file2 outfile)))

(defun file-compare (file1 file2 outfile)
  (with-open-file (output-stream outfile :direction :output :if-exists :supersede)
    (let ((*package* *package*) l1 p1 l2 p2)
      (multiple-value-setq (l1 p1) (read-lists-from file1))
      (multiple-value-setq (l2 p2) (read-lists-from file2))
      (cond
       ((and (consp p1) (not (consp (cdr p1)))
             (consp p2) (not (consp (cdr p2)))
             (eq (first p1) (first p2)))
        (setf *package* (first p1))
        (print `(in-package ,(package-name (first p1))) output-stream))
       (t (warn "~S has packages ~S, while~%~S has packages ~S." file1 p1 file2 p2)))
    (list-compare l1 l2 output-stream))))

(defun read-lists-from (infile)
  (with-open-file (input-stream infile :direction :input)
    (let ((*package* *package*) (lists ()) (packages ()) read)
      (until (eq :eof (setq read (read input-stream nil :eof)))
        (when (and (listp read) (eq (first read) 'in-package))
          (newl packages (eval read)))
        (newl lists read))
      (values (nreverse lists) (nreverse packages)))))

(defun similar-exprs? (l1 l2)
  (and (listp l1) (listp l2) (eq (first l1) (first l2))
       (listp (cdr l1)) (listp (cdr l2)) (eq (second l1) (second l2))))

(defun list-compare (l1 l2 &optional (out *standard-output*))
  (let (pl1 expr1 pl2)
    (setq l1 (delete nil l1)
          l2 (delete nil l2)
          pl1 l1)
    (format out "~%~%;; Equal expressions")
    (while pl1
      (if (or (null (setq expr1 (first pl1)))
              (not (setq pl2 (member expr1 l2 :test #'equal))))
        (pop pl1)
        (progn
          (print expr1 out)
          (setf (car pl1) (cadr pl1)
                (cdr pl1) (cddr pl1)
                (car pl2) (cadr pl2)
                (cdr pl2) (cddr pl2)))))
    (format out "~%~%;; Similar expressions with differences")
    (setq l1 (delete nil l1)
          l2 (delete nil l2)
          pl1 l1)
    (while pl1
      (if (or (null (setq expr1 (first pl1)))
              (not (setq pl2 (member expr1 l2 :test #'similar-exprs?))))
        (pop pl1)
        (progn
          (format out "~%~%;; - ~S" (second expr1))
          (print expr1 out)
          (print (first pl2) out)
          (setf (car pl1) (cadr pl1)
                (cdr pl1) (cddr pl1)
                (car pl2) (cadr pl2)
                (cdr pl2) (cddr pl2)))))
    (setq l1 (delete nil l1)
          l2 (delete nil l2))
    (format out "~%~%;; Expressions of 1st list")
    (mapc #'(lambda (e) (print e out)) l1)
    (format out "~%~%;; Expressions of 2nd list")
    (mapc #'(lambda (e) (print e out)) l2)
    out))

;; =============================================================================-======
;; Expression prefixer
;; See tests at the end

;; ==== operation level ====

(defstruct
  (level
   (:print-function
    (lambda (me stream depth)
      (declare (ignore depth))
      (format
       stream
       "#<ops=~S	; ~:[not~;   ~] associative ~:[~; ;  default=~:*~S~]~
       ~:[~; 	;   inverse=~:*~S~]~:[~;	;   translations=~:*~S~]>"
       (level-ops me)
       (level-associative? me)
       (level-dop me)
       (level-iop me)
       (level-l-op.tr me)))))
  dop           ;default operators
  iop           ;inverse operators
  tr-default    ;default operator translation
  tr-inverse    ;inverse operator translation
  ops           ;all operators
  l-op.tr       ;translations
  associative?)

(defun create-level (dop iop l-op.tr associative? &aux me ops)
  "Create a level object from the specification of the default and inverse operators
and the associativity."
  (unless (listp dop) (setq dop (list dop)))
  (unless (listp iop) (setq iop (list iop)))
  (setq me
     (make-level
       :dop dop
       :iop iop
       :ops (setq ops (remove-duplicates (append dop iop (mapcar #'car l-op.tr))))
       :l-op.tr l-op.tr
       :associative? associative?))
  (setf (level-tr-default me) (level-translate me (car (level-dop me)))
        (level-tr-inverse me) (level-translate me (car (level-iop me))))
  (mapc
   #'(lambda (op)
       (check-type op symbol)
       (unless (eq (symbol-package op) #.(find-package "KEYWORD"))
         (import op "COMMON-LISP")
         (export op "COMMON-LISP")))
   ops)
  me)

(defun level-has? (me op) (memq op (level-ops me)))
(defun level-default? (me op) (memq op (level-dop me)))
(defun level-inverse? (me op) (memq op (level-iop me)))
(defun level-default (me) (first (level-dop me)))
(defun level-translate (me op) (or (cassq op (level-l-op.tr me)) op))

;; ==== globals and macros ====

(progn
  (defvar *levels*)
  (defvar *all-ops*)
  
  (setf *levels*
        (list
         (create-level '\; () '((\; . progn)) t)
         (create-level ':= () '((:= . setq)) nil)
         (create-level '(==) () '((== . =)) t)
         (create-level '(!=) () '((!= . /=)) t)
         (create-level '(>) () () t)
         (create-level '(<) () () t)
         (create-level '(>=) () () t)
         (create-level '(<=) () () t)
         (create-level '+ '- () t)
         (create-level '* '/ () t)
         (create-level '(** ) () '((** . expt) ) nil)))
  
  (setf *all-ops* (apply #'append (mapcar #'level-ops *levels*))))

(defvar *default-operation* '*)

(eval-when (eval compile)
  (defmacro operation? (op) `(memq ,op *all-ops*)))

;; ==== help, user function and sharp macro character ====

(defun prefix-help ()
  "Describes the current levels of operations."
  (format t "~&The current levels of operations are:")
  (mapc 'print *levels*)
  (values))

(defun prefix-expr (expr)
  "Converts an (usually infixed) expression into a lisp (prefixed) expression.
Help on available operations can be obtained with (prefix-help)."
  (cond
   ((not (consp expr)) expr)
   ((and (symbolp (first expr)) (fboundp (first expr)))
    `(,(first expr) ,.(mapcar #'prefix-expr (rest expr))))
   (t (prefix-iexpr (test-syntax expr) *levels*))))

(set-dispatch-macro-character
 #\# #\i
 #'(lambda (stream char count)
     (declare (ignore char count))
     (prefix-expr (read stream t nil t)))) ; maybe (CLtLII p548)

;; ==== internals ====
(defun sp-simplify (exprs ops level)
  ;; (length exprs) = (length ops) + 1
  (cond
   ;; only 1 elt
   ((null ops) (first exprs))
   ;; not associative (** ^): left to right
   ((not (level-associative? level))
    (let ((result (nextl exprs)))
      (mapc
       #'(lambda (op expr)
           (setq result (list (level-translate level op) result expr)))
       ops exprs)
      result))
   ;; associative and only inverse (- /)
   ((every #'(lambda (op) (level-inverse? level op)) ops)
    `(,(level-tr-inverse level) ,.exprs))
   ;; associative operator (+ - * /): skip default operators (+ *)
   ;; could use commutativity too...
   (t `(,(level-tr-default level)
        ,(nextl exprs)
        ,.(mapcar
           #'(lambda (op expr)
               (if (level-default? level op) expr
                   (list (level-translate level op) expr)))
           ops exprs)))))



(defun prefix-iexpr (iexpr levels)
  ;; iexpr == (expr op expr op ... expr)
  (if (endp (rest iexpr))
    (prefix-expr (first iexpr))
    (let* ((exprs ())
           (ops ())
           (level (first levels))
           sub-iexpr)
      (while iexpr
        (setq sub-iexpr (list (nextl iexpr)))
        (while (and iexpr (not (level-has? level (first iexpr))))
          (newl sub-iexpr (nextl iexpr))
          (newl sub-iexpr (nextl iexpr)))
        (newl exprs (prefix-iexpr (nreverse sub-iexpr) (cdr levels)))
        (when iexpr (newl ops (nextl iexpr))))
      (sp-simplify (nreverse exprs) (nreverse ops) level))))



(defun test-syntax (expr)
  "1st level syntax test and completion with * when omitted."
  (let ((orig-expr expr)
        (result ())
        (operation? nil)
        elt)
    (while expr
      (setq elt (first expr))
      (newl
       result
       (if operation?
         (if (operation? elt) (nextl expr) *default-operation*)
         (if (operation? elt)
           (if (eq orig-expr expr)
             (error
              "Syntax: the infixed expression should not begin with an operation:~%~S"
              orig-expr)
             (error
              "Syntax: The operations ~S and ~S should not be consecutive ~
               in the infixed expression:~%~S"
              (first result) elt orig-expr))
           (nextl expr))))
      (setq operation? (not operation?)))
    (unless operation?
      (error "Syntax: the infixed expression should not end with an operation:~%~S"
              orig-expr))
    (nreverse result)))

;; ==== global and user function ====

(defvar *compile-num-lambda* t)

;; (make-num-fun '(y := z - x \; y * z + y) nil )
;; => '(lambda (x z) (let (y) (progn (setq y (- z x)) (+ (* y z) y))))
;; but
;; (make-num-fun '(f(z x)= y := z - x \; y * z + y) nil)
;; => '(lambda (z x) (let (y) (progn (setq y (- z x)) (+ (* y z) y))))

(defun make-num-fun (fexpr)
  "Creates a lisp function object from the \"functional\" expr <fexpr> which is
basically an infixed expression (see prefix-expr and prefix-help).
When <fexpr> begins with something like (f(x)= ...), the formal arguments are taken
from the given list, otherwise they are deduced from the body of <fexpr> and collected
in the order they appear in it.
Local variables are automatically handled.
The resulting function is compiled when the value of *compile-num-lambda* is T (default)."
  ;; fexpr == <expr> || (<fun> <args> = . <expr>)
  (multiple-value-bind (lambda name) (make-num-lambda fexpr)
    (if *compile-num-lambda*
      (compile name lambda)
      lambda)))

(defun make-num-lambda (fexpr)
  "Creates a lisp function object from the \"functional\" expr <fexpr> which is
basically an infixed expression (see prefix-expr and prefix-help).
When <fexpr> begins with something like (f(x)= ...), the formal arguments are taken
from the given list, otherwise they are deduced from the body of <fexpr> and collected
in the order they appear in it.
Local variables are automatically handled.
The resulting function is a lambda list not compiled."
  ;; fexpr == <expr> || (<fun> <args> = . <expr>)
  (let ((=? (and (consp fexpr) (consp (cdr fexpr)) (consp (cddr fexpr))
                 (symbolp (first fexpr))
                 (listp (second fexpr))
                 (eq '= (third fexpr))))
        (name ()) args expr rvars wvars)
    (when =?
      (setq name (nextl fexpr) args (nextl fexpr))
      (nextl fexpr))
    (setq expr (prefix-expr fexpr))
    (multiple-value-setq (rvars wvars)
      (if =? (rw-vars expr args) (rw-vars expr)))
    (values
     `(lambda ,rvars ,(if wvars `(let ,wvars ,expr) expr))
     name)))

;; ==== internals ====

(defun rw-vars (expr &optional (args () args-p))
  (let ((*rvars* (reverse args)) (*wvars* ()))
    (declare (special *rvars* *wvars*))
    (rw-vars-expr expr)
    (setq *rvars* (nreverse *rvars*))
    (when (and args-p (not (equal args *rvars*)))
      (warn "Found other free variables ~S not in ~S~%in expression ~S."
            (set-difference *rvars* args) args expr)
      (setq *rvars* args))
    (values *rvars* *wvars*)))

(defun rw-vars-expr (expr)
  (declare (special *rvars* *wvars*))
  (cond
   ((null expr))
   ((and (symbolp expr) (not (constantp expr)))
    (unless (or (memq expr *rvars*) (memq expr *wvars*))
      (newl *rvars* expr)))
   ((not (consp expr)))
   ((eq 'setq (car expr))
    (mapc #'rw-vars-expr (cddr expr))
    (unless (or (memq (second expr) *rvars*) (memq (second expr) *wvars*))
      (newl *wvars* (second expr))))
   (t (mapc #'rw-vars-expr (cdr expr)))))

;; =============================================================================-======



(defmethod filter-aux ((self C-spdata) condexpr  slot)
  (when (spd-debug self) (format t "expr ~D~%" condexpr))
  (let ( condflags tmp)
    (setf condflags (mapcar (eval `(function ,condexpr)) (funcall slot self))) ;; apply filter to slot and collect in flag list
    (when (spd-debug self)  (format t "result ~D~%" condflags))
    (dolist (varslot '(freqs amps bws phases weights partials))
      ;(format t "varslot ~D data ~D~%" varslot (funcall varslot self))
      (when (slot-value self varslot) ;; if slot is not empty, then it must have length = size
        (setf tmp (slot-value self varslot))
        (setf (slot-value self varslot) ())
        (mapc  #'(lambda (flag x)
                   (when flag (setf (slot-value self varslot) (push x (slot-value self varslot)))) ;; if filter test was t, put parameter in slot
                   ) condflags tmp)
        (setf (slot-value self varslot) (reverse (slot-value self varslot))))
      )
    self)
  )





(defmethod filter ((self C-spdata) fct val slot)
  (assert (> (size self) 0) () "filter: cannot filter a spdata of null size")
  (when (spd-debug self) (format t "~4Fs: test ~D with val ~D on slot ~D~%" (frame self) fct val slot))
    (let ( (to-spdata (copy-spdata self)) condexpr (msgbp "for band-pass filter input value must be a pair (inf sup) or a list of pairs ((inf1 sup1) (inf2 sup2) ..)"))       
      (ecase fct  ;; building filtering expression
        (band-pass (assert (listp val) (val) msgbp)
                   (cond ((member-if #'listp val) (assert (and (not (member-if-not #'listp val))
                                                               (not (member-if-not #'(lambda(x) (if (= (length x) 2) t ())) val))) (val)
                                                          msgbp)      
                          
                          (setf condexpr ())
                          ; construct expression (or (and (x >= min) (x <= max)) (and ....)) for all band-pass values
                          (dolist (bp val) (setf condexpr (append condexpr `((and (>= x ,(car bp)) (<= x ,(cadr bp)))))))
                          (setf condexpr (make-num-lambda (cons 'or condexpr)))
                          (filter-aux to-spdata condexpr  slot))
                         (t (setf condexpr (make-num-lambda `(and (>= x ,(car val)) (<= x ,(cadr val)))))
                            (filter-aux  to-spdata condexpr  slot))))
        (low-pass (assert (numberp val) (val) "value for low pass must be a number")
                  (setf condexpr (make-num-lambda `(<= x ,val)))
                  (filter-aux to-spdata condexpr  slot))
        (high-pass (assert (numberp val) (val) "value for low pass must be a number")
                   (setf condexpr (make-num-lambda `(> x ,val)))
                   (filter-aux to-spdata condexpr  slot))
        (reject-band (assert (listp val) (val) msgbp)
                     (cond ((member-if #'listp val) (assert (and (not (member-if-not #'listp val))
                                                               (not (member-if-not #'(lambda(x) (if (= (length x) 2) t ())) val))) (val)
                                                          msgbp)      
                          
                          (dolist (bp val) (setf condexpr (make-num-lambda `(or (<= x ,(car bp)) (>= x ,(cadr bp)))))
                                  (filter-aux to-spdata condexpr  slot)))
                         (t (setf condexpr (make-num-lambda `(and (<= x ,(car val)) (>= x ,(cadr val)))))
                            (filter-aux self condexpr val slot))))
        (eq (cond ((listp val)
                     (setf condexpr (make-num-lambda `(when (member  x  ',val) t)))                         
                           (filter-aux to-spdata condexpr  slot ))
                  (t (setf condexpr (make-num-lambda `(= x ,val)))
                      (filter-aux to-spdata condexpr  slot))
                  ))
        (neq (cond ((listp val) 
                     (setf condexpr (make-num-lambda `(when (not (member x ',val)) t)))
                           (filter-aux to-spdata condexpr  slot))
                  (t (setf condexpr (make-num-lambda `(/= x ,val)))
                      (filter-aux to-spdata condexpr  slot))
                  ))
        )  
      (setf (frame to-spdata) (frame self))
      (setf (typ to-spdata) (typ self))
      (print (freqs to-spdata))
      (setf (size to-spdata) (length (freqs to-spdata)))
      (setf (file to-spdata) (concatenate 'string (file self) "  filtered"))
      (when (<= (size to-spdata) 0) (ccl::beep) (warn "filter: output spdata of null size"))
      to-spdata)
    )


(defmethod filter ((self C-spdata-seq) fct val slot)
  (make-instance 'C-spdata-seq :duration (duration self)
                               :typ (typ self) :file (file self)
                               :spdata (mapcar #'(lambda (x) (filter x fct val slot))
                                               (spdata self)))
)


(defmethod tm-scale ((self C-spdata-seq) fct val slot)
  (make-instance 'C-spdata-seq :duration (duration self)
                               :typ (typ self) :file (file self)
                               :spdata (mapcar #'(lambda (x) (tm-scale x fct val slot))
                                               (spdata self)))
)




(defmethod setup ((self C-spdata) freqs amps bws phases partials weights typ file)
      (setf (freqs self) freqs)
      (setf (amps self) amps)
      (setf (bws self) bws)
    (setf (size self) (length (freqs self)))
    (setf (phases self) phases)
    (setf (partials self) partials)
    (setf (weights self) weights)
    (setf (typ self) typ)
    (setf (file self) file)
self)

(defmethod reset  ((self C-spdata)) 
  (setup self () () () () () () 'empty ())
self)

(om::defmethod! filter-spdata ((spdata t)
                       (fct symbol)
                       (val t) 
                       (slot symbol))
:initvals (list '() "band-pass" '() "amps")
 :indoc '("object" "filter" "val" "slot")
 :icon 160
:menuins '((1 (("band-pass" 'band-pass) 
                ("low-pass" 'low-pass) 
                ("high-pass" 'high-pass)
                ("reject-band" 'reject-band)
                ("neq" 'neq)))
          (3 (("amps" 'amps) 
                ("freqs" 'freqs) 
                ("partials" 'partials)
                ("weights" 'weights)
                ("bws" 'bws)
                ("size"  'size))))
 :doc "creates a new spdata object with only partials data satisfying the test"
       


  (when spdata
    (cond ((listp spdata) (mapcar #'(lambda(x) (filter-spdata x fct val slot)) spdata)) ;; recursive call in case val is list
          ;;((listp val) (mapcar #'(lambda(x) (filter spdata fct x slot)) val))
          (t (filter spdata fct val slot))) ;; spdata is spdata or spdata-seq
    )
)



;;;*******************************mask-read******************************************

(defun arith0 (dep pas n)
  (let ((L ()))
    (dotimes (i n (reverse L))
      (push (+ (* pas i) dep) L))))

(defmethod read-mask-data ((self C-spdata-seq) filename beg end &optional nmax)
  (cond ((and beg end) (assert (> end beg) () "mask-read: beg must be > end"))
        (beg (assert (>=  beg 0) () "mask-read: beg must be >= 0"))
        (end (assert (>  end 0) () "mask-read: end must be > 0"))
        )
  (with-open-file (in filename :direction :input)
    (let (curline readframe cursize curmask-frame  (continueflag t) (calculeflag ()))
      (while (and continueflag (setf cursize (read in nil nil)))
        
        (setf cursize (round (if nmax (min nmax cursize) cursize)))  ;si limitation du nombre de pics (sera peut-être modifié par les developpeurs)
        
        (setf readframe (read in))
        ;;;;;(format t "readfdrame : ~a~%" readframe)
        (cond ((and (not end) (not beg)) ;; if beg and end nil, read it all
               (setf calculeflag t)
               )
              ((and end (not beg)) ;; start from 0 until specified end in ms
               (setf calculeflag (<= readframe end)))
              ((and beg end)
               (setf calculeflag  (cond ((< readframe beg) ())
                                        ((<= readframe end )  t)
                                        (t  ())))
               )
              ((and (not end) beg)
               (setf calculeflag (>= readframe beg)))
              )
        (when (and calculeflag (> cursize 0.0))
          (setf curmask-frame (make-instance 'C-spdata)) 
          (format t "~Ds : ~D partials~%" readframe cursize)
          )
        (dotimes (i cursize)
          (cond (calculeflag  ;; when partial to read, push items in lists, if not, read all line and dump
                 ;;; (setf (partials curmask-frame) (cons (read in) (partials curmask-frame)))
                 (setf (freqs curmask-frame) (cons (read in) (freqs curmask-frame)))
                 (setf (amps curmask-frame) (cons (read in) (amps curmask-frame)))
                 (setf (weights curmask-frame) (cons (read in) (weights curmask-frame))))
                (t (setf curline (read-line in))) ;; dump because out of interval beg-end
                )
          )
        (when (and calculeflag (> cursize 0))
          curline  ;for no more warnings
          (setf (frame curmask-frame) readframe)
          (setf (size curmask-frame) cursize)
          (setf (partials curmask-frame) (arith0 1 1 cursize))
          (setf (freqs curmask-frame) (reverse (freqs curmask-frame)))
          (setf (amps curmask-frame) (reverse (amps curmask-frame)))
          (setf (weights curmask-frame) (reverse (weights curmask-frame)))
          (setf (bws curmask-frame) (make-list (length (amps curmask-frame)) :initial-element 1))  ; lp 20/8/94
          ;(setf normalized-amps (normalize-amp amps))
          ;(setf (phases curmask-frame) (reverse (phases curmask-frame)))
          (setf (spdata self) (cons curmask-frame (spdata self)))
          )
        )
      (setf (spdata self) (reverse (spdata self)))
      
      (when beg ;; if begin time was specified, adjust all time frame values
        (when (= beg (frame (first (spdata self)))) 
          (setf beg (- beg (- (first (spdata self)) (second (spdata self))))))
        (mapc #'(lambda (x) (setf (frame x) (- (frame x) beg))) (spdata self))
        )   
      
      (setf (file self) (mac-namestring filename))  
      (setf (typ self) 'mask) 
      (setf (duration self) (- (frame (car (last (spdata self)))) (frame (first (spdata self)))))  
      (format t "finished reading additive synthesis  file ~D from time ~5F to ~5F duration ~5F ~%" filename 
              (if beg beg 0) (if end end readframe) (duration self))
      ))
  self)


(om::defmethod! mask-read ((filename t)
                   &optional (beg '()) (end '())
                   (nmax '()))
        


 :initvals (list '() '() '() '())
 :indoc '("filename" "beg" "end" "nmax")
 :icon 135
 :doc "reads mask analysis data and returns a spdata object (c-spdata-seq class)"
  (let ((spdata-seq (make-instance 'C-spdata-seq )))
    (unless filename 
      (setf filename (om-choose-file-dialog :directory  *lastspfile*
                                             :button-string "mask file")))
    (when  filename 
      (setf *lastspfile* filename)
       (read-mask-data spdata-seq filename beg end nmax)
      )
    )
  )


;;*************************************par-spdata******************************************






       
 (om::defmethod! par-spdata ((spdata t)
                             (menu symbol))
   
   
   :initvals (list '() "freqs")
   :indoc '("spdata" "menu" )
   :icon 160
   :menuins '((1 (("freqs" 'freqs)
                 ("amps" 'amps)
                 ("bws" 'bws)
                 ("partials" 'partials)
                 ("weights" 'weights)
                 ("phases" 'phases)
                 ("size" 'size))))
   :doc "get data in spdata object from the specified slot"
   
   (when (or (and (atom spdata) (spd-debug spdata))
             (and (listp spdata) (spd-debug (first spdata)))) (format t "values for menu ~D~%" menu))
   (let (ret-list)
     (cond ((listp spdata) (setf ret-list (remove nil (mapcar #'(lambda(x) (funcall menu x)) spdata))))
           ((typep spdata 'C-spdata-seq) 
            (setf ret-list (remove nil (mapcar #'(lambda(x) (list (frame x) (funcall menu x))) (spdata spdata)) :key 'cadr))
            (when ret-list (setf ret-list (mat-trans ret-list))))
           (t (setf ret-list (funcall menu spdata))))
     ret-list))




;*****************************FILTER ALL**********************************************


(defun filtre-position (list poslist)
  (reverse (set-difference list (loop for i in poslist collect (nth i list)))))
      
;(filtre-position '(1 2 3 4 5 6 7) '(1 3))
    


(defmethod* losfiltre ((self c-spdata)  par test)
  (let (rejected newspdata)
    (loop for i from 0 to (- (size self) 1 ) do
          (unless (funcall test (nth i (par-spdata self par)))
            (push i rejected)))
    (setf newspdata  (make-instance 'c-spdata
                       :size (- (size self) (length rejected))
                       :frame (frame self)
                       :partials (filtre-position (partials self) rejected)
                       :freqs (filtre-position (freqs self) rejected)
                       :amps (filtre-position (amps self) rejected)
                       :bws (filtre-position (bws self) rejected)
                       :phases (filtre-position (phases self) rejected)
                       :weights (filtre-position (weights self) rejected)
                       ))
    (setf (normalized-amps newspdata) (filtre-position (normalized-amps self) rejected))
    newspdata))

(defmethod* losfiltre ((self list)  par test)
  (loop for item in self collect  (losfiltre item par test)))

(defmethod* losfiltre ((self c-spdata-seq)  par test)
  (make-instance 'c-spdata-seq 
    :spdata (losfiltre (spdata self) par test)
    :duration 0 ))


 
(om::defmethod! filter-all (self par test format)
   
   :initvals (list '() "freqs" '() "list")
   :indoc '("spdata" "menu" "test" "format")
   :icon 160
   :menuins '((1 (("freqs" 'freqs)
                 ("amps" 'amps)
                 ("bws" 'bws)
                 ("partials" 'partials)
                 ("weights" 'weights)
                 ("phases" 'phases)
                 ("size" 'size)))
             (3 (("list" 'list)
                 ("object" 'object))))
   :doc "blabla"
  
(ecase format
(list (par-spdata (losfiltre self par test) par))
(object (losfiltre self par test))))


 ;***********************************************************************************************    
 
; additions 4/10/04

(om::defmethod! mk-spdata ((freqs t ) 
                      (amps t ) 
                      (phases t ) 
                      (partials t )
                      (weights t )
                      (bws t))
       
:initvals '( 0 0 0 0 60 1)
:icon 160
:doc "creates a new spdata object or a list of spdata objects. Inputs are simple list to create
a single object or a list of lists to create a series of spdata objects. See sp-data-seq"

  (let (spdata llsizes)
    ; looking for double lists (list of lists) in order : freqs, amps, bws, phases
    ; collecting list of sublist sizes if double list, () otherwise
    (setf llsizes (or (ll-length freqs)
                      (ll-length amps)
                      (ll-length bws) 
                      ( ll-length phases)
                      ))

    (cond (llsizes ;; at least one list of lists --> sequence of spectra
    ;; preparing data lists; in order freqs, amps, bws, phases, partials, weights
    (setf freqs (fill-if-not-ll "freqs" freqs llsizes))
    (setf amps (fill-if-not-ll "amps" amps llsizes))
    (setf bws (fill-if-not-ll "bws" bws llsizes))
    (setf phases (fill-if-not-ll "phases" phases llsizes))
    (setf weights (fill-if-not-ll "weights" weights llsizes))
    (setf partials (fill-partials-ll partials llsizes))
           ; loop to create list of spdata
;(format t "llflag ~D bw ~D~%" llflag bws)
           (dotimes (i (length llsizes))
           (setf spdata (cons (make-instance 'C-spdata :freqs (nth i freqs)
                                                     :amps  (nth i amps)
                                                     :phases (nth i  phases)
                                                     :partials  (nth i  partials)
                                                     :bws (nth i bws)
                                                     :weights (nth i weights)
                                                     :size (length (nth i freqs))
                                                     )
                              spdata)))
           (setf spdata (reverse spdata)))
           (t ;; all inputs are numbers or simple lists --> only one spectrum
            (setf llsizes (or (listp-size freqs) (listp-size amps) ; set size to list size if any
                            (listp-size bws) (listp-size phases) 
                            (listp-size weights)))
            (when (not llsizes) (error "mk-spdata: all inputs are numbers. At least one list is needed"))
            (setf spdata (make-instance 'C-spdata :freqs (fill-if-not-l freqs llsizes)
                                                     :amps (fill-if-not-l amps llsizes)
                                                     :phases (fill-if-not-l phases llsizes)
                                                     :partials (fill-if-not-l partials llsizes)
                                                     :bws (fill-if-not-l bws llsizes)
                                                     :weights (fill-if-not-l weights llsizes)
                                                     :size llsizes
                                                     ))
            ))
    spdata)
)




(om::defmethod! mk-spdata-seq ((frames list ) 
                      (spdata list))
                      
       
:initvals '( '() '())
:icon 160
:doc "creates a spdata-seq object with a list of spdata objects and a list of frames"

  (when spdata
    (unless (listp spdata) (setf spdata (list spdata)))
    (when (< (length frames) (length spdata)) ;;when frames list is too short
      ;(setf frames (append frames (make-list (- (length spdata) (length frames))
      ;                                   :initial-element (car (last frames)))))
      (warn "frames list is too short")  ; lp 20/8/94
            )
  ;  ;; converts in seconds, cumulate for absolute time and store in frame slot
  ;  (let (atimes (cumtime 0))
  ;    (setf atimes (cons 0 (mapcar #'(lambda( time ) (setf cumtime (+ (/ time 100.) cumtime))) frames)))
    ;; write frame slot in spdata
      (mapcar #'(lambda (spd time) (setf (frame spd) time)) spdata frames)   ; lp 20/8/94

    (make-instance 'C-spdata-seq :duration (car (last frames))
                   :spdata spdata :typ (typ (first spdata))
                   :file (file (first spdata)))
    )
)



;  ===================== interpolations de spectres =================================


; ---------  fct utiles de pw:apparier-clos-sp  -----------

(defclass liste-double ()
  ((LdeP :initform () :initarg :LdeP :accessor LdeP)
   (lg :initform () :initarg :lg :accessor lg)
   (ld :initform () :initarg :ld :accessor ld)
   (nbre-P :initform 1 :initarg :nbre-P :accessor nbre-P)))

;(defclass liste-multiple (liste-double)
;  ((nbre-instts :initform 2 :accessor nbre-instts)))

(defun make-ldouble (L1 L2)
    (make-instance 'liste-double :Lg L1
                 :Ld L2))

;(defun make-lmultiple (L1 L2)
;    (make-instance 'liste-multiple :Lg L1
;                 :Ld L2))

;(defun make-LdeP (LdeP)
;  (make-instance 'liste-double :LdeP LdeP
;                 :nbre-P (length LdeP)))

;(defun xmake-LdeP (LdeP)
;  (make-instance 'liste-multiple :LdeP LdeP
 ;                :nbre-P (length LdeP)))

(defmethod optim1-l ((self liste-double) i)  ;;((10 8) (10 9) (10 12) (10 0)) 
 (let ((liste (nth i (LdeP  self))))
   (do* ((n 0 (1+ n))
         (ind 0 ))
        ((eq n (length liste))(nth ind liste))
    (setf ind (if (< (diff-de-P   (nth n liste))
                     (diff-de-P (nth ind liste)))
                n ind)))))

(defmethod apari ((self liste-double) seuil)
  (let ((L1 (lg self))(L2 (ld self))(L ()))
    (dolist (elt L1 (setf (LdeP  self)(reverse L)))
      (if (find-elt-seuil elt l2 seuil)
        (push (find-elt-seuil  elt l2 seuil) L)))))

(defmethod appariement ((self liste-double) seuil) 
  (apari  self seuil)
  (optim1-apari   self)
  (optim2-apari  self)
  (recup-elts self)
(LdeP self))

(defmethod  optim1-apari ((self liste-double))  ;et multiple
    (do ((n 0 (1+ n)) L)
        ((eq n (length (LdeP self))) (setf (LdeP  self) (reverse L)))
      (push (optim1-l  self n) L)))

(defmethod optim2-apari ((self liste-double))
  (let ((L ())
        (P ())
        (D ()))
    (while (setq P (pop (LdeP self)))
      (setf D (car (LdeP self)))
      (if (equal (max 1 (second P))
              (second D))              ;->deux pareils consécutifs
        (if (< (diff-de-P  P)
               (diff-de-P D))             ;si le premier écart est + faible
          (and (pop (LdeP  self)) (push P (LdeP self)))())
        (push P L))) (setf (LdeP self) (reverse L))))

(defmethod appariement2 ((self liste-double) seuil type)
  "type = type du seuil puis type de la liste"
  (let ((l1 (lg self))
        (l2 (ld self)))
    (cond ((equal type '("midic" "midic"))
           (setf (LdeP self) 
                 (f->mc2 
                  (appariement 
                   (make-ldouble (mc->f2 L1)
                                 (mc->f2 L2)) 
                   seuil))))
          ((equal type '("Hz" "midic"))
           (setf (LdeP self) 
                 (f->mc2 
                  (appariement 
                   (make-ldouble (mc->f2 L1)
                                 (mc->f2 L2)) 
                   (- 0 seuil)))))
          ((equal type '("Hz" "Hz"))
           (appariement self (- 0 seuil)))
          (t
           (appariement self seuil)))
    (LdeP self)))

(defmethod compar-apari ((objf liste-double) (obj2 liste-double))
  (let* (( LdeP2 ())
         (L1 (lg obj2))
         (L2 (ld obj2))
         (n1 (taille (car L1)))
         (n2 (taille (car L2))))
      (dolist (Paire (LdeP objf) (setf (LdeP obj2) (reverse LdeP2)))
        (cond ((zerop (moy-dt-x (car Paire)))
               (push (cons-x-elt (pop L2) 1 n1) LdeP2))
              ((zerop (moy-dt-x (second Paire)))
               (push (cons-x-elt (pop L1) 0 n2) LdeP2))
              (t
               (push (list (pop L1)(pop L2)) LdeP2))))))


(defmethod insert-si-absent ((self liste-double) numero rang n)
  (declare (ignore n))
  (let ((elt (nth numero (if (eq rang 0) (lg self)(ld self))))
        (Lelt ()) stockage)
    (push elt Lelt)
    (while Lelt
      (let ((Paire (pop (LdeP self))))
        (cond ((null Paire) (push (cons-x-paire (pop Lelt) rang) stockage))
              ((equal (nth rang Paire) elt)
               (and (pop Lelt)(push Paire stockage)))
              ((> (moy-dt-x Paire) elt)
               (and (push (cons-x-paire (pop Lelt) rang) stockage)
                    (push Paire stockage)))
              (t (push Paire stockage)))))
    (setf (LdeP self) (append (reverse stockage) (LdeP self)))))


(defmethod recup-elts ((self liste-double))  
  (do ((n 0 (1+ n)))
      ((eq n (length (Lg self))) ()) (insert-si-absent self n 0 1))
  (do ((n 0 (1+ n)))
      ((eq n (length (Ld self))) ()) (insert-si-absent self n 1 1)))


(defmethod intpola1 ((self liste-double) scaler)
  (let (Lamps
        (Lamps1 (Lg self))
        (Lamps2 (Ld self)))
    (dolist (P (LdeP self) (reverse Lamps))
      (if (< (length (flat P)) 3)
        (push (scal-Pair-dtx   P scaler) Lamps)
        (cond ((zerop (moy-dt-x  (car P)))
               (push (* scaler (pop Lamps2)) Lamps))
            ((zerop (moy-dt-x (second P)))
             (push (* (- 1 scaler) (pop Lamps1)) Lamps))
            (t
             (push (abs (+ (* (- 1 scaler)(pop Lamps1))
                           (* scaler (pop Lamps2)))) Lamps)))))))

(defmethod intersecta ((self liste-double) scaler)
  (let (Lamps
        (Lamps1 (Lg self))
        (Lamps2 (Ld self)))
    (dolist (P (LdeP self) (reverse Lamps))
      (cond ((zerop (moy-dt-x (car P)))(pop Lamps2))
            ((zerop (moy-dt-x (second P)))(pop Lamps1))
            (t
             (push (abs (+ 
                         (* (- 1 scaler) (pop Lamps1))
                         (* scaler (pop Lamps2)))) Lamps))))))

(defmethod intpolamps ((self liste-double) scaler flag)
  (let ((scalspec (cond 
                   ((< scaler 0.25) 0)
                   ((< scaler 0.5)(- (* 4 scaler) 1))
                   ((< scaler 0.75) 1)
                   (t (- 4 (* 4 scaler))))))
    (cond ((= flag 0) ;elt gauche seult 
           (lg self))
          ((or (= flag 1) (= flag 4)); intersec + mixage
           (intpola1  self scaler))
          ((or (= flag 2)(= flag 5)) ; mixage (pas d'appariements)
           (append (rescale  (lg self)(- 1 scaler))
                   (rescale (ld self) scaler)))
          ((= flag 3) ; intersec seult (pas de freq isolées)
           (intersecta self scaler))
          ((= flag 6) ; = scaler <0.5 instg sinon instd pour freqs
           (if (< scaler 0.5) (lg self)(ld self)))
          ((= flag 7) ; double interpol pour les freqs
           (intpola1 self scalspec)))))

(defmethod intpolf1 ((self liste-double) scaler)
  (let (L)
    (dolist (P (LdeP self) (reverse L))
      (push (if (zerop (min (moy-dt-x(car P))(moy-dt-x(cadr P))))
              (max (moy-dt-x(car P))(moy-dt-x(cadr P)))
            (+ (* (- 1 scaler) (car P))(* scaler (cadr P)))) L ))))

(defmethod intersect ((self liste-double) scaler)
  (let (L)
    (dolist (P (LdeP self) (reverse L))
      (if (zerop (min (moy-dt-x(car P))(moy-dt-x(cadr P))))
        ()
        (push (+ (* (- 1 scaler) (car P))(* scaler (cadr P))) L)))))

(defmethod intpolfreqs ((self liste-double) scaler flag)
  (let ((scalspec (cond 
                   ((< scaler 0.25) 0)
                   ((< scaler 0.5)(- (* 4 scaler) 1))
                   ((< scaler 0.75) 1)
                   (t (- 4 (* 4 scaler))))))
    (cond ((= flag 0) ;elt gauche seult 
           (lg self))
          ((or (= flag 1)(= flag 4)) ; intersec + mixage
               (intpolf1 self scaler))
          ((or (= flag 2)(= flag 5)) ; mixage (pas d'appariements)
               (append (lg self) (ld self)))
          ((= flag 3) ; intersec seult (pas de freq isolées)
           (intersect  self scaler))
          ((= flag 6) ; = scaler <0.5 instg sinon instd pour freqs
           (if (< scaler 0.5) (lg self) (ld self)))
          ((= flag 7) ; double interpol pour les freqs
           (intpolf1  self scalspec)))))


(defmethod intpolbw1 ((self liste-double) scaler)
   (let (Lbws
        (Lbws1 (Lg self))
        (Lbws2 (Ld self)))
    (dolist (P (LdeP self) (reverse Lbws))
      (cond ((zerop (moy-dt-x (car P))) (push (pop Lbws2) Lbws))
            ((zerop (moy-dt-x (second P))) (push (pop Lbws1) Lbws))
            (t (push (scalbw (pop Lbws1)(pop Lbws2) scaler) Lbws))))))


(defmethod intersectbw ((self liste-double) scaler)
  (let (Lbws
        (Lbws1 (Lg self))
        (Lbws2 (Ld self)))
    (dolist (P (LdeP self) (reverse Lbws))
      (cond ((zerop (moy-dt-x (car P)))(pop Lbws2))
            ((zerop (moy-dt-x (second P)))(pop Lbws1))
            (t (push (scalbw (pop Lbws1)(pop Lbws2) scaler) Lbws))))))

(defmethod intpolbwres
       ((self liste-double) scaler resmoy1/2) ;-->les freq appariées restent interpolées 
  (let (Lbws
        (Lbws1 (Lg self))
        (Lbws2 (Ld self)))
   (dolist (P (LdeP self) (reverse Lbws))
     (cond ((zerop (moy-dt-x (car P)))(push (* (pop Lbws2) 
                                    (expt resmoy1/2 (- scaler 1))) Lbws)) 
           ((zerop (moy-dt-x (second P))) (push (* (pop Lbws1)
                                        (expt resmoy1/2 scaler)) Lbws))
           (t (push (scalbw (pop Lbws1)(pop Lbws2) scaler) Lbws))))))


(defmethod intpolbws
  ((self liste-double) scaler flag resmoy1/2)
  (let ((scalspec (cond 
                   ((< scaler 0.25) (* 4 scaler))
                   ((< scaler 0.5) 1)
                   ((< scaler 0.75)(- 3 (* 4 scaler)))
                   (t 0))))
    (cond  ((= flag 0) ;elt gauche seult 
            (lg self))
           ((= flag 1) ; intersec + mixage
            (intpolbw1 self scaler))
           ((= flag 2) ; mixage (pas d'appariements)
            (append (lg self)(ld self)))
           ((= flag 3) ; intersec seult (pas de freq isolées)
            (intersectbw self scaler))
           ((= flag 4) ; = resmoy !
            (intpolbwres self scaler resmoy1/2))
           ((= flag 5) ; = type2 pour les freq
            (append (rescale (lg self) (expt resmoy1/2 scaler))
                    (rescale (ld self) (expt resmoy1/2 (- scaler 1)))))
           ((= flag 6) ; = scaler <0.5 instg sinon instd pour freqs
            (if (< scaler 0.5) (rescale (lg self) (expt resmoy1/2 (* 2 scaler)))
                (rescale (ld self) (expt resmoy1/2 (- 1 (* 2 scaler)))))) ;-->à vérifier
           ((= flag 7) ; double interpol pour les freqs
            (intpolbwres self scalspec resmoy1/2)))))



; ---------  fct utiles de pw:sp-fonctions  -----------

(defun f->mc2 (LorA)
  (cond ((null LorA)
          ())
        ((atom LorA)
         (if (numberp LorA) (round (f->mf  LorA)) LorA))
        (t
         (cons (f->mc2 (car LorA))(f->mc2  (cdr LorA))))))



(defun mc->f2 (LorA)
  (cond ((null LorA)
          ())
        ((atom LorA)
        (if (numberp LorA) (mc->f LorA) LorA))
        (t
         (cons (mc->f2 (car LorA))(mc->f2 (cdr LorA))))))



 
(defun flat2 (lorx)
  (if (atom lorx) lorx (flat lorx)))


(defun taille (LorA)
  (if (atom LorA) 1 (length LorA)))

(defun rescale (Lamps scaler)
  (let ((l () ))
    (dolist (elt Lamps (reverse L))
      (push (* elt scaler) L))))

(defun scal-Pair-dtx (Paire scaler)
  (let ((A (car Paire))
        (B (second Paire)))
    (setf A (if (numberp A) A 0)
          B (if (numberp B) B 0))
    (abs (+ (* (- 1 scaler) A)(* scaler B)))))

(defun scalbw (bw1 bw2 scaler)
"donne la largeur de bande moyenne (inverse) entre bw1 et bw2 pondéré par scal"
  (/ (* bw1 bw2)(+ (* (- 1 scaler) bw2)(* scaler bw1))))

(defun find-elt-seuilmc (elt l2 seuil)
  (let ((L ())
        (elt1 (f->mc2 elt)))
    (dolist (elt2 l2 (reverse L))   
      (if (<= (abs (- elt1 (f->mc2 elt2))) seuil) 
        (setf L (cons (list elt elt2) L))
        ()))))


(defun find-elt-seuilf (elt l2 seuil)
  (let ((L ()))
    (dolist (elt2 l2 (reverse L))   
      (if (<= (abs (- elt elt2)) seuil) 
        (setf L (cons (list elt elt2) L))
        ()))))

(defun find-elt-seuil (elt l2 seuil)
  (if (> 0 seuil) (find-elt-seuilf  elt l2 (- seuil))
      (find-elt-seuilmc  elt l2 seuil)))

(defun diff-de-P (Paire)
  (abs (- (car Paire)(cadr Paire))))

(defun cons-x-elt (elt rang n)
(cond ((null elt )())
      ((eq rang 0)
       (if (eq n 1)(cons elt '(x))
           (cons elt (list (make-list n :initial-element 'x)))))
      (t (if (eq n 1)(cons 'x (cons elt ()))
             (list (make-list n :initial-element 'x) 
                   elt)))))

(defun cons-x-paire (elt rang)
  (if (null elt)()
      (if (eq rang 0)(cons elt (cons 'x ()))
          (cons 'x (cons elt ())))))

(defun som-dt-x (l)
  (if l (+ (if (equal (car l) 'x) 0 (car l))(som-dt-x (cdr l))) 0))

(defun moy-dt-x (l)
  (if (atom l) (if (numberp l) l 0)
      (let ((l (remove 0 (remove 'x (flat l)))))
        (/ (som-dt-x l)(length l)))))


(defun res-moy (Lamps Lbws)
    (do* ((n 0 (1+ n))
          (sumres (* (car Lamps)(expt (/ 1 (car Lbws)) 2))
                  (+ sumres (* (nth n Lamps) (expt (/ 1 (nth n Lbws)) 2))))
          (sumamp (car Lamps)(+ sumamp (nth n Lamps))))
         ((eq n (1- (length Lamps)))(expt (/ sumres sumamp) 0.5))))


; ---------  interpolation de spectres ------------------------------------------




(om::defmethod! intpol-model ((spd1 t)
                       (spd2 t)
                       (seuil number)      ; 100
                       (scaler number)     ; 0 
                       (flag integer)            ;(:value 1  :min-val 0 :max-val 7))
                       &optional 
                       (th-type    'midic)   ;  (:menu-box-list (("midic" . "midic") ("Hz" . "Hz"))
                       (mod-type  'midic )) ; menu-box-list (("midic" . "midic") ("Hz" . "Hz"))))
  :initvals '( '() '() 100 0  1 'midic 'midic)
  :menuins '((5 (("midic" 'midic) ("hz" 'hz) )) (6 (("midic" 'midic) ("hz" 'hz) )))
  :icon 160
  :doc "donne un nouveau modèle interpolé à partir de deux modèles de départ, 
les frames sont calculées sur le modèle gauche (1)
flag : entier, entre 1 et 7"
  (let* (Lfg Lfd Lag Lad Lbwg Lbwd  resmoy1/2
             objetf objeta objetbw)
    (setf Lfg  (freqs  spd1))
    (setf Lag  (amps spd1 ))
    (if (zerop (apply '+ Lag))(setf Lag (arith0 0.00001 0 (length Lfg)))) ;;new lp 28/9/94
    (setf Lbwg  (bws spd1))
    (setf Lfd  (freqs  spd2))
    (setf Lad  (amps spd2 ))
    (if (zerop (apply '+ Lad))(setf Lad (arith0 0.00001 0 (length Lfd)))) ;;new lp 28/9/94
   (setf Lbwd  (bws spd2))
    (setf objetf (make-ldouble Lfg Lfd))
    (setf objeta (make-ldouble Lag Lad))
    (setf objetbw (make-ldouble Lbwg Lbwd))
    (progn
      (setf resmoy1/2  (/ (res-moy  (lg objeta) (lg objetbw))
                         (if (zerop (res-moy (ld objeta) (ld objetbw))) 1
                             (res-moy (ld objeta) (ld objetbw))))) ;; a ameliorer si amps = 0
      (appariement2 objetf seuil (list th-type mod-type) ) ;; --> old 31/11/93 lp
      ;(appariement3 objetf seuil (list th-type mod-type) flag) -> modif bof
      (compar-apari  objetf objeta)
      (compar-apari objetf objetbw)
      ;(list (intpolfreqs objetf scaler flag)  --> old 19/8/93 lp
      ;            (intpolamps objeta scaler flag)
      ;            (intpolbws objetbw scaler flag resmoy1/2))
      (when (and objetf objeta objetbw)
        (let ((modele (make-instance   'C-spdata ))(liste-ref (intpolfreqs objetf scaler flag)))
          (setup modele liste-ref (intpolamps  objeta scaler flag)
                 (intpolbws objetbw scaler flag resmoy1/2)
                 (make-list  (length liste-ref) :initial-element 0)
                 (arith0 1 1 (length liste-ref)) (make-list  (length liste-ref) :initial-element 0)
                 'empty ()) 
          (setf (frame modele) (frame  spd1))        
          modele)
        ))))









;;------------------------- list utilities -------------------------------------------


(defun fill-if-not-l ( obj size)
  (cond ((not obj) (error "null input in mk-spdata (fill-if-not-l )"))
        ((listp obj) 
         (cond ((= (length obj) size) obj)
               ((< (length obj) size) 
                (append obj (make-list (- size (length obj)) :initial-element (car (last obj)))))
               ((> (length obj) size) (subseq obj 0 size))
               ))
        (t ;; should be number
         (make-list size :initial-element obj)))
)

;; test if list of list. returns list of sublists length if list of lists, length if simple length
(defun ll-length (obj)
  (cond ((not obj) ()) ;; null object
        ((and (listp obj) (not (member-if-not #'listp obj))) (mapcar #'length obj)) ;; list of lists
        ((listp obj) ()) ;; simple list
        ((numberp obj) ())
        (t ()))
)

;; if not list of list, make a list repeating element
(defun fill-if-not-ll (varname obj sizes)
  (let ((lle (ll-length obj)))
    (cond (lle (unless (equal lle sizes) (error "bad format for ~D: ~D instead of ~D~%" varname lle sizes))
               obj)
          ((listp obj) (check-parlist (make-list (length sizes) :initial-element obj) sizes))
          (t (mapcar #'(lambda(x) (make-list x :initial-element obj)) sizes))
        )
    )
)

;; adjust list of lists; obj is a list of lists and sizes is a list of sizes
(defun check-parlist (obj sizes)
  (when (or (not (listp obj)) (not (listp sizes)) (not (= (length obj) (length sizes))))
    (error "check-parlist: bad formats for ~D and ~D~%" obj sizes))
  (let (curemt result cursize curlength)
  (dotimes (i (length obj))
    (setf curemt (pop obj))
    (setf cursize (nth i sizes))
    (setf curlength (length curemt))
    (setf curemt (cond ((< curlength cursize) ;; list in list of lists is too short
           (append curemt (make-list (- cursize curlength) :initial-element (car (last curemt)))))
          ((> curlength cursize) (subseq curemt 0 cursize)) ;; current element too long
          (t curemt)))
    (setf result (cons curemt result)))
  (setf result (reverse result))
  result)
)


(defun fill-partials-ll ( obj sizes)
  (let ((lle (ll-length obj)))
    (cond (lle (unless (equal lle sizes) (error "bad format for ~D: ~D instead of ~D~%" "partials" lle sizes))
               obj)
          ((listp obj) (check-partial-list (make-list (length sizes) :initial-element obj) sizes))
        (t (mapcar #'(lambda(x) (arithm-ser 1 1 x)) sizes))
        )
    )
)

;; adjust list of lists; obj is a list of lists and sizes is a list of sizes
(defun check-partial-list (obj sizes)
  (when (or (not (listp obj)) (not (listp sizes)) (not (= (length obj) (length sizes))))
    (error "check-parlist: bad formats for ~D and ~D~%" obj sizes))
  (let (curemt result cursize curlength)
  (dotimes (i (length obj))
    (setf curemt (pop obj))
    (setf cursize (nth i sizes))
    (setf curlength (length curemt))
    (setf curemt (cond ((< curlength cursize) ;; list in list of lists is too short
           (append curemt (arithm-ser (car (last curemt)) 1 (+ (car (last curemt)) (- cursize curlength) ))))
          ((> curlength cursize) (subseq curemt 0 cursize)) ;; current element too long
          (t curemt)))
    (setf result (cons curemt result)))
  (setf result (reverse result))
  result)
)



;; returns list size is a list
(defun listp-size (obj)
  (cond ((listp obj) (length obj))
        (t ())
        )
)




