
(in-package :om)




;;
;; need to allow linefeed for end of line as well as newline
;;
(defun getline (input-stream eof-error-p eof-value)
  (declare (optimize (speed 3) (safety 1)))
  (if (stream-eofp input-stream)
    (if eof-error-p
      (error 'end-of-file :stream input-stream)
      (values eof-value (or eof-value t)))
    (let ((char nil)
          (str (make-array 20
                           :element-type 'base-character
                           :adjustable t :fill-pointer 0)))
      ;(multiple-value-bind (reader reader-arg) (stream-reader input-stream)
        (while (and (setq char (read-char input-stream nil nil))   ; (funcall reader reader-arg))
                         (not (eq char #\newline))
                         (not (eq char #\linefeed)))
               ;;; !!! CCL
               ;(when (and (not (base-character-p char)) (base-string-p  str))
               ;  (setq str (ccl-string-to-extended-string str)))
          (vector-push-extend char str))
       ; )
      (values str (null char)))))


;; first -- list of times
;; second -- list of freq frames
;; third -- list of amp frames
;; fourth -- list of index frames


(om::defmethod!  spear-write ( times indices frequencies amplitudes  &optional (filename nil))
  
  :initvals (list nil nil nil nil nil)
  :icon 128
  :doc ""
  
  (setf filename (if (equal filename 'nil) (om-choose-new-file-dialog ) filename))
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (let ((num-frames (length times)))
      (format file "par-text-frame-format~c" #\linefeed)
      (format file "point-type index frequency amplitude~c" #\linefeed)
      (format file "frame-count ~d~c" num-frames #\linefeed)
      (format file "frame-data~c" #\linefeed)
      (loop for time in times
            for freqs in frequencies
            for amps in amplitudes
            for inds in indices
            do
            (format file "~d ~d" time (length freqs))
            (loop for freq in freqs
                  for amp in amps
                  for index in inds
                  do
                  (format file " ~d ~d ~d" index freq amp))
            (format file "~c" #\linefeed)))))





#|

(let ((data (read-spear-text-format "HDN:Users:mkling:Desktop:Earth62Noisy.sdif.txt" 100)))
  (write-spear-text-format "HDN:Users:mkling:Desktop:foo.txt"
                           (first data)
                           (second data)
                           (third data)
                           (fourth data)))
|#

(defmethod read-spear-spdata ((self C-spdata-seq) filename beg end &optional nmax)
  (with-open-file (input filename :direction :input)
    (let* ((valid-format t)
           (header
            (loop with reading-header = t
                  for i from 0
                  while (and valid-format reading-header)
                  collect
                  (let* ((line (getline input nil nil))
                         (line-list (loop with stream = (make-string-input-stream line)
                                          for item = (read stream nil :eof)
                                          while (not (eq item :eof))
                                          collect item)))
                    (cond
                     ((and (= i 0) (not (eq (first line-list) 'par-text-frame-format)))
                      (progn
                        (setf valid-format nil)
                        nil))
                     ((eq (first line-list) 'frame-data)
                      (progn
                        (setf reading-header nil)
                        line-list))
                     (t 
                      line-list))))))
      (when (first header)
        (let* ((point-type (cdr (assoc 'point-type header :test 'eq)))
               (requested-types '(index frequency amplitude))
               (requested-length (length requested-types))
               (avail-types (intersection requested-types point-type))
               (point-length (length point-type))
               (frame-count (car (cdr (assoc 'frame-count header :test 'eq))))
               point-indexing
               time point-count current-frame)
          (when (= (length requested-types) (length avail-types))
            (setf point-indexing
                  (loop for pt in point-type
                        collect
                        (position pt requested-types)))
            
            ;;; Now read the frames
            ;;; should worry about eof's
            (loop for fr from 0 below frame-count
                  ;;; read time
                  for time = (read input nil :eof)
                  for before-end = (or (not end) (and end (<= time end)))
                  for after-beg = (or (not beg) (and beg (>= time beg)))
                  for store = (and after-beg before-end)
                  while before-end
                  do
                  ;;;;;;;;;;;;;;;;;; (push time (elt all-data 0))
                  (setf point-count (read input nil :eof))
                  ;;;; make a data structure to store the data for this frame
                  (when store (setf current-frame (make-instance 'C-spdata)))
                  (loop for ix from 0 below point-count
                        do
                        (loop for ip in point-indexing
                              for value = (read input nil :eof)
                              if store do
                              (cond ((= ip 0)
                                     ;; index type
                                     (push value (partials current-frame)))
                                    ((= ip 1)
                                     ;; frequency type
                                     (push value (freqs current-frame)))
                                    ((= ip 2)
                                     (push value (amps current-frame))))))
                  (when store
                    (setf (size current-frame) point-count)
                    ;;;; frame appears to be the time value of this frame
                    (setf (frame current-frame) time)
                    (setf (partials current-frame) (nreverse (partials current-frame)))
                    (setf (freqs current-frame) (nreverse (freqs current-frame)))
                    (setf (amps current-frame) (nreverse (amps current-frame)))
                    ;;;; what is this????
                    (setf (weights current-frame) (make-list point-count :initial-element 1))
                    (setf (bws current-frame) (make-list point-count :initial-element 1))
                    (push current-frame (spdata self))))
            (setf (spdata self) (nreverse (spdata self)))
            
            (setf (file self) (namestring filename))
            (setf (typ self) 'mask)
            (let ((end-time (frame (car (last (spdata self)))))
                  (start-time (frame (first (spdata self)))))
              (setf (duration self) (- end-time start-time))  
              (format t "finished reading SPEAR additive synthesis file ~D from time ~5F to ~5F duration ~5F ~%" filename 
                      start-time end-time (duration self)))
            
            (when beg ;; if begin time was specified, adjust all time frame values
              ;;(when (= beg (frame (first (spdata self))))
              ;; I think this is wrong --
              ;;  (setf beg (- beg (- (first (spdata self)) (second (spdata  self))))))
              (mapc  #'(lambda (x) (setf (frame x) (- (frame x) beg))) (spdata self)))
            
            self))))))




          
#|
(pprint (read-spear-text-format "HDN:Users:mkling:Desktop:Earth62Noisy.sdif.txt" 4))

(read-spear-text-format "HDN:Users:mkling:Desktop:Frequency.dmg")
|#

#|
(om::defmethod! spear-read ((filename t)
                            &optional (beg nil) (end nil) (nmax nil))
  :initvals (list nil nil nil nil)
  :indoc '("filename" "beg" "end" "nmax")
  :icon 128
  :doc "reads SPEAR analysis data from a text file and returns a spdata object (c-spdata-seq class)"
  (let ((spdata-seq (make-instance 'C-spdata-seq)))
    (unless filename
      (setf filename (CCL:choose-file-dialog :directory  *lastspfile*
                                             :button-string "SPEAR text file")))
    (when  filename
      (setf *lastspfile* filename)
      (read-spear-spdata spdata-seq filename beg end nmax)
      )
    )
  )

|#


; :directory  *lastspfile*   pose probleme quand on change d'environnement


(om::defmethod! spear-read ((filename t)
                            &optional (beg nil) (end nil) (nmax nil))
  :initvals (list nil nil nil nil)
  :indoc '("filename" "beg" "end" "nmax")
  :icon 128
  :doc "reads SPEAR analysis data from a text file and returns a spdata object (c-spdata-seq class)"
  (let ((spdata-seq (make-instance 'C-spdata-seq)))
    (unless filename
      (setf filename (om-choose-file-dialog 
                                             :button-string "SPEAR text file")))
    (when  filename
      (setf *lastspfile* filename)
      (read-spear-spdata spdata-seq filename beg end nmax)
      )
    )
  )




#|
(defun read-spear-text-format (filename &optional (num-frames 5))
  (with-open-file (input filename :direction :input)
    (let* ((valid-format t)
           (header
            (loop with reading-header = t
                  for i from 0
                  while (and valid-format reading-header)
                  collect
                  (let* ((line (getline input nil nil))
                         (line-list (loop with stream = (make-string-input-stream line)
                                          for item = (read stream nil :eof)
                                          while (neq item :eof)
                                          collect item)))
                    (cond
                     ((and (= i 0) (neq (first line-list) 'par-text-frame-format))
                      (progn
                        (setf valid-format nil)
                        nil))
                     ((eq (first line-list) 'frame-data)
                      (progn
                        (setf reading-header nil)
                        line-list))
                     (t 
                      line-list))))))
      (when (first header)
        (let* ((point-type (cdr (assq 'point-type header)))
               (requested-types '(index frequency amplitude))
               (requested-length (length requested-types))
               (avail-types (intersection requested-types point-type))
               (point-length (length point-type))
               (frame-count (car (cdr (assq 'frame-count header))))
               time point-count frame-data point-indexing
               all-data
               )
          (when (= (length requested-types) (length avail-types))
            (setf point-indexing
                  (loop for pt in point-type
                        collect
                        (position pt requested-types)))
            
            (setf all-data (make-list (1+ requested-length)))
                    
            (loop for fr from 0 below (min num-frames frame-count)
                  ;;; Now read the frames
                  ;;; should worry about eof's
                  ;;; read time                 
                  do
                  (setf time (read input nil :eof))
                  (push time (elt all-data 0))
                  collect
                  (progn
                    ;;(setf time (read input nil :eof))
                    (setf point-count (read input nil :eof))
                    (setf frame-data (make-list (length requested-types)))
                    (loop for ix from 0 below point-count
                          do
                          (loop for ip in point-indexing
                                for value = (read input nil :eof)
                                do 
                                (when ip
                                  (push value (elt frame-data ip)))))
                    (loop for ix from 0 below requested-length
                          do
                          (push (nreverse (elt frame-data ix)) (elt all-data (1+ ix))))))
            (loop for ix from 0 to requested-length
                  do
                  (setf (elt all-data ix) (nreverse (elt all-data ix))))
            all-data))))))
|#



;=================================  matching ======================================================






(om::defmethod! matching ((dates list) (frqs  list) (amps  list) (spatia  list)
                                    (fichier list))  
:initvals (list '(1 2)  '(1 2) '(1 2)  '(1 2) nil)
   :indoc '("dates"  "frqs" "amps" "spatia" "fichier")
   :icon 128
   :doc ""


  (let ((nom (or fichier (choose-new-file-dialog)))
        (prev-frame nil)
        (prev-time nil))
    
    (flet ((output-frame (fil time frame)
             (format fil "~f " time)
             (loop for ix from 0 below (length prev-frame)
                   for elem = (elt prev-frame ix)
                   do
                   (format fil "~f ~f ~f ~d " (first elem) (second elem) (third elem) (sixth elem)))
             (format fil "~%")))
      
      (when nom
        (with-open-file (file nom :direction :output 
                              :if-exists :supersede)
          ;; magic number
          (format file "omTr~%")
          
          (loop with track-count = 0
                for cur-time in dates
                for cur-freq-frame in frqs
                for cur-amp-frame in amps
                for cur-pan-frame in spatia
                for cur-frame = (make-array (length cur-freq-frame))
                do
                ;; genreate format of new frame as an array
                (loop for ix from 0 
                      for freq in cur-freq-frame
                      for amp in cur-amp-frame
                      for pan in cur-pan-frame
                      do
                      ;; format is freq, amp, pan, forward index, backward index
                      (setf (elt cur-frame ix) (list freq amp pan nil nil nil)))
                
                (when prev-frame
                  ;; if previous frame, match from previous forward to new frame
                  (loop for pix from 0 below (length prev-frame)
                        for prev = (elt prev-frame pix)
                        do
                        (loop for cix from 0 below (length cur-frame)
                              for cur = (elt cur-frame cix)
                              for cents = (abs (* 1200 (log (/ (first cur) (first prev)) 2)))
                              for other-match = nil
                              do
                              ;;; check distance in frequency
                              (when (< cents 125)
                                ;;; see if this is already claimed match
                                (if (fourth prev)
                                  ;;; previous already matched forward
                                  (setf other-match (abs (* 1200
                                                            (log (/ (first prev)
                                                                    (first (elt cur-frame (fourth prev)))) 2))))
                                  (when (fifth cur)
                                    ;;; current is already matched backward to another peak
                                    (setf other-match (abs (* 1200
                                                              (log (/ (first cur)
                                                                      (first (elt prev-frame (fifth cur)))) 2))))))
                                
                                (when (or (not other-match)
                                          (< cents other-match))
                                  
                                  ;;; connect previous to current
                                  ;;; clear other connections
                                  (when (fourth prev)
                                    ;;; previous already points forward
                                    ;;; clear the backward reference from the forward reference
                                    (setf (fifth (elt cur-frame (fourth prev))) nil)
                                    (setf (sixth (elt cur-frame (fourth prev))) nil))
                                  (when (fifth cur)
                                    ;;; current already points backward
                                    ;;; clear the forward reference from the backward reference
                                    (setf (fourth (elt prev-frame (fifth cur))) nil))
                                  
                                  
                                  ;;; forward match
                                  (setf (fourth prev) cix)
                                  ;;; backward match
                                  (setf (fifth cur) pix)
                                  ;;; track index
                                  (setf (sixth cur) (sixth prev))
                                  ))))
                  ;; unmatched forward in previous frame are deaths (insert into current?)
                  ;; unmatched back in current frame are births (insert into previous?)
                  
                  ;; now output the previous frame
                  (output-frame file prev-time prev-frame))
                
                ;; fill in unmatched back in current frame with birth index
                (loop for ix from 0 below (length cur-frame)
                      for cur = (elt cur-frame ix)
                      if (not (sixth cur))
                      do
                      (setf (sixth cur) track-count)
                      (incf track-count))
                
                
                (setf prev-frame cur-frame)
                (setf prev-time cur-time))
          
          (when prev-frame
            (output-frame file prev-time prev-frame))
          )))))


;;; defines the data structure for a single partial
(defstruct (sp-partial (:constructor
			make-partial
			(&optional (size 0)
				   (time (make-array size :element-type 'float))
				   (freq (make-array size :element-type 'float))
				   (amp (make-array size :element-type 'float)))))
  (size 0 :type integer)
  (time (make-array 0 :element-type 'float) :type array)
  (freq (make-array 0 :element-type 'float) :type array)
  (amp (make-array 0 :element-type 'float) :type array))

(defun sp-partial-start (par)
  (elt (sp-partial-time par) 0))

(defun sp-partial-end (par)
  (elt (sp-partial-time par) (- (sp-partial-size par) 1)))

(defun sp-partial-dur (par)
  (- (sp-partial-end par) (sp-partial-start par)))

;; times of all partials as a list of lists
(defun sp-partials-times (partials)
  (loop for par in partials
        collect (coerce (sp-partial-time par) 'list)))

;; frequencies of all partials as a list of lists
(defun sp-partials-freqs (partials)
  (loop for par in partials
        collect (coerce (sp-partial-freq par) 'list)))

;; amplitudes of all partials as a list of lists
(defun sp-partials-amps (partials)
  (loop for par in partials
        collect (coerce (sp-partial-amp par) 'list)))
  
;; read a spear file in par-text-partials format
(defun read-sp-partials (filename)
    (with-open-file (input filename :direction :input)
    (let* ((partials nil)
	   (valid-format t)
           (header
            (loop with reading-header = t
	       for i from 0
	       while (and valid-format reading-header)
	       collect
	       (let* ((line (read-line input nil nil))
		      (line-list (loop with stream = (make-string-input-stream line)
				    for item = (read stream nil :eof)
				    while (not (eq item :eof))
				    collect item)))
		 (cond
		   ((and (= i 0) (not (eq (first line-list) 'par-text-partials-format)))
		    (progn
		      (setf valid-format nil)
		      nil))
		   ((eq (first line-list) 'partials-data)
		    (progn
		      (setf reading-header nil)
		      line-list))
		   (t 
		    line-list))))))
      (when (first header)
        (let* ((point-type (cdr (assoc 'point-type header)))
               (requested-types '(time frequency amplitude))
               ;;(requested-length (length requested-types))
               (avail-types (intersection requested-types point-type))
               ;;(point-length (length point-type))
               (partials-count (car (cdr (assoc 'partials-count header))))
               point-indexing
	       current-partial)
          (when (= (length requested-types) (length avail-types))
            (setf point-indexing
                  (loop for pt in point-type
		     collect
		     (position pt requested-types)))
            
;;; Now read the partials
;;; should worry about eof's
            (loop for pr from 0 below partials-count
;;; read index
	       for index = (read input nil :eof)
;;; read length
	       for point-count = (read input nil :eof)
;;; read start time
	       for start-time = (read input nil :eof)
;;; read end time
	       for end-time = (read input nil :eof)
	       for store = t
	       do
;;;; make a data structure to store the data for this frame
	       (when store (setf current-partial (make-partial point-count))
		     start-time
		     end-time
		     index
		     (loop for ix from 0 below point-count
			do
			(loop for ip in point-indexing
			   for value = (read input nil :eof)
			   if store do
			   (cond
			     ((= ip 0)
			      ;; time type
			      (setf (elt (sp-partial-time current-partial) ix) (coerce value 'float)))
			     ((= ip 1)
			      ;; frequency type
			      (setf (elt (sp-partial-freq current-partial) ix) (coerce value 'float)))
			     ;; amplitude type
			     ((= ip 2)
			      (setf (elt (sp-partial-amp current-partial) ix) (coerce value 'float)))))))
		 (when store
		   (push current-partial partials))))))
      (nreverse partials))))


(om::defmethod! spear-read-partials ((filename t))
  :initvals (list nil)
  :indoc '("filename")
  :icon 128
  :doc "reads SPEAR analysis data in partials format from a text file and returns a list of sp-partials"
  (unless filename
    (setf filename (om-choose-file-dialog 
                    :button-string "SPEAR text file")))
  (when filename
    (read-sp-partials filename)))

(defun write-sp-partials (filename partials)
  (format t "Writing partials...")
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (let ((num-partials (length partials)))
      (write-partials-header file num-partials)
      (loop for par in partials	   
	 for ix from 0
	 do
	   (write-partial file par ix))))
  (format t "done~%"))

(defun write-partials-header (file count)
  (format file "par-text-partials-format~c" #\linefeed)
  (format file "point-type time frequency amplitude~c" #\linefeed)
  (format file "partials-count ~d~c" count #\linefeed)
  (format file "partials-data~c" #\linefeed))

(defun write-partial (file partial index)
  (let* ((times (sp-partial-time partial))
	 (freqs (sp-partial-freq partial))
	 (amps (sp-partial-amp partial))
	 (length (min (length times) (length freqs) (length amps))))
    (when (> length 0)
      (format file "~d ~d ~d ~d~c" index length
	      (elt times 0)
	      (elt times (1- length))
	      #\linefeed)
      (format file "~d ~d ~d" (elt times 0) (elt freqs 0) (elt amps 0))
      (loop for i from 1 below length
	 do (format file " ~d ~d ~d" (elt times i) (elt freqs i) (elt amps i)))
      (format file "~c" #\linefeed))))

(om::defmethod! spear-write-partials ((partials t) (filename t))
  :initvals (list nil nil)
  :indoc '("partials" "filename")
  :icon 128
  :doc "write SPEAR analysis data in partials format"
  (unless filename
    (setf filename (om-choose-new-file-dialog)))
  (write-sp-partials filename partials))

(om::defmethod! spear-make-partials ((times list) (frequencies list) (amplitudes list))
  :initvals (list nil nil nil)
  :icon 128
  :doc "make a list of sp-partials suitable for writing using spear-write-partials"
  (loop for time-l in times
        for freq-l in frequencies
        for amp-l in amplitudes
        collect
        (make-partial (length time-l) (coerce time-l 'vector) (coerce freq-l 'vector) (coerce amp-l 'vector)))) 

(defun copy-partial (partial)
  (make-partial (sp-partial-size partial)
		(copy-seq (sp-partial-time partial))
		(copy-seq (sp-partial-freq partial))
		(copy-seq (sp-partial-amp partial))))

(defun offset-partial (partial offset &optional (copy t))
  (let ((np (if copy (copy-partial partial)
		partial)))
    (loop for i from 0 below (sp-partial-size np)
       do
	 (incf (elt (sp-partial-time np) i) offset))
    np))

(defun transpose-partial (partial interval-ratio &optional (copy t))
  (let ((np (if copy (copy-partial partial)
		partial)))
    (loop for i from 0 below (sp-partial-size np)
       for f = (elt (sp-partial-freq np) i)
       do
	 (setf (elt (sp-partial-freq np) i) (* f interval-ratio)))
    np))

(defun transpose-partial-env (partial interval-ratio strength-env &optional (copy t))
  (let ((np (if copy (copy-partial partial)
		partial)))
    (loop for i from 0 below (sp-partial-size np)
       for f = (elt (sp-partial-freq np) i)
	   for time = (elt (sp-partial-time np) i)
       do
	 (setf (elt (sp-partial-freq np) i)
		   (* f (+ 1.0 (* (interpl time strength-env) (- interval-ratio 1.0))))))
    np))

(defun stretch-partial (partial timescale &optional (copy t))
  (let* ((np (if copy (copy-partial partial)
		 partial))
	 (start (sp-partial-start np)))
    (loop for i from 0 below (sp-partial-size np)
       for time = (elt (sp-partial-time np) i)
       do
	 (setf (elt (sp-partial-time np) i)
	       (+ start (* timescale (- time start)))))))

(defun time-scale-partial (partial timescale &optional (copy t))
  (stretch-partial partial timescale copy))

(defun amplitude-scale-partial (partial ampscale &optional (copy t))
  (let ((np (if copy (copy-partial partial)
		partial)))
    (loop for i from 0 below (sp-partial-size np)
       for a = (elt (sp-partial-amp np) i)
       do
	 (setf (elt (sp-partial-amp np) i) (* a ampscale)))
    np))

(defun ambitus-scale-partial (partial scaling average-freq &optional (copy t))
  (let* ((np (if copy (copy-partial partial)
		 partial))
	 (minf (loop for i from 0 below (sp-partial-size np)
		  for f = (elt (sp-partial-freq np) i)
		  minimize f))
	 (maxf (loop for i from 0 below (sp-partial-size np)
		  for f = (elt (sp-partial-freq np) i)
		  maximize f))
	 (newmin (- average-freq (* (- average-freq minf) scaling)))
	 (newmax (+ average-freq (* (- maxf average-freq) scaling))))
    (loop for i from 0 below (sp-partial-size np)
       for f = (elt (sp-partial-freq np) i)
       do
	 (if (/= minf maxf)
	     (setf (elt (sp-partial-freq np) i) (rescale f minf maxf newmin newmax))))
    np))

;; assumes amplitude envelope from 0 to 100!
(defun amp-env-partial (partial env &optional (copy t))
  (let* ((np (if copy (copy-partial partial)
		 partial))
	 (start (sp-partial-start np))
	 (end (sp-partial-end np))
	 (envmin (first env))
	 (envmax (first (last env 2))))
    (loop for i from 0 below (sp-partial-size np)
       for time = (elt (sp-partial-time np) i)
       for pamp = (elt (sp-partial-amp np) i)
       for amp = (* pamp (interpl (rescale time start end envmin envmax) env))
       do
	 (setf (elt (sp-partial-amp np) i) amp))))

;;(defun ampmax-env-partial (partial env &optional (copy nil))
  ;; TODO

(defun sp-partial-avg-freq (partial)
  (let* ((freqs (sp-partial-freq partial))
	 (len (length freqs)))
    (/ (loop for i from 0 below len
	  summing
	  (elt freqs i)) len)))

(defun sp-partial-avg-amp (partial &optional (points nil))
  (let* ((amps (sp-partial-amp partial))
	 (len (if points (min points (length amps)) (length amps))))
    (/ (loop for i from 0 below len
	  summing
	  (elt amps i)) len)))

(defun sp-partials-avg-freqs (partials)
  (loop for par in partials collect (sp-partial-avg-freq par)))

(defun sp-partials-avg-amps (partials)
  (loop for par in partials collect (sp-partial-avg-amp par)))

