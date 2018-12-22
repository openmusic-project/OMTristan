
;;;===================
;;; OM-Tristan
;;;===================

; new methods for existing OM functions
; !! compatibility

(in-package :om)

(defmethod* approx-m  ((self chord) (approx number) &optional (ref-midic 0))
  (ch-modif self  '= (approx-m (lmidic self) approx) 'lmidic) )

(defmethod* approx-m  ((self chord-seq) (approx number) &optional (ref-midic 0))
  (ch-modif self  '= (approx-m (lmidic self) approx) 'lmidic) )

(defmethod* approx-m  ((self multi-seq) (approx number) &optional (ref-midic 0))
   (mki 'multi-seq
     :chord-seqs (approx-m (chord-seqs self)  approx)))


(defmethod* approx-m  ((self multi-seq) (approx list) &optional (ref-midic 0))
   (mki 'multi-seq
     :chord-seqs (double-mapcar 'approx-m (chord-seqs self)  approx)))



; pour multiseq   -    voir si ce n'est pas dangereux !!!
(defmethod ldur ((self multi-seq))
  (mapcar 'ldur (chord-seqs self)))

(defmethod lvel ((self multi-seq))
  (mapcar 'lvel (chord-seqs self)))

(defmethod lmidic ((self multi-seq))
  (mapcar 'lmidic (chord-seqs self)))

(defmethod loffset ((self multi-seq))
  (mapcar 'loffset (chord-seqs self)))

(defmethod lchan ((self multi-seq))
  (mapcar 'lchan (chord-seqs self)))

(defmethod lonset ((self multi-seq))
  (mapcar 'lonset (chord-seqs self)))

(defmethod lport ((self multi-seq))
  (mapcar 'lport (chord-seqs self)))

(defmethod chords ((self multi-seq))
  (flat (mapcar 'chords (chord-seqs self))))



; om+ se comporte comme un transpositeur de midics
(defmethod om+ ((self container) (num number))
  (ch-modif  self  '+ num 'lmidic))


; om* se comporte comme stretch
(defmethod om* ((self chord-seq) (num t))
  (seq-stretch  self   num ))

(defmethod om* ((self multi-seq) (num number))
  (seq-stretch  self   num ))

(defmethod om* ((self chord) (num t))
  (chord-stretch  self   num ))


(defmethod om* ((self multi-seq) (num list))
  (seq-stretch  self   num ))


; permet de créer des listes structurées
(defmethod create-list ((count list) (elem t))
  (loop for c in count
        collect (create-list c elem)))



; ajout des ports  2-10-03    - ajout de port-modif.pfsl dans dossier patches
; attention:
; arithm-ser (ordre paramètres changé)  
;list-filter (param. differents), table-filter (fct differente)   utiliser filtre-liste, multi-filter
; om-mean (n'accepte pas trees)    utiliser tm-average

; --------------------------------------------------------------------------------------
; pour éviter approximation au demi-ton dans align-chords
; pour éviter que legato soit mis à 100 dans align-chords et merger


#|

(defun tm-make-quanti-chords (note-list delta)
  (loop while note-list
        for note = (car note-list)
        with pitch-list and dur-list  and offset-list and chan-list and vel-list
        with base-time = (second (first note-list))
        if (<= (- (second (first note-list)) base-time) delta)
        do 
        
        (push  (first note) pitch-list)
        (push (third note) dur-list)
        (push (fifth note) chan-list)
        (push (fourth note) vel-list)
        (push (- (second note) base-time) offset-list)
        (pop note-list)
        else
        collect (mk-chord-at  base-time  pitch-list dur-list offset-list chan-list vel-list ) into result
        and do (setf base-time (second note) pitch-list () dur-list ()   offset-list () chan-list () vel-list () )
        finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list  ))))))
 
(defmethod tm-chord-seq->mf-info ((self chord-seq))
    (loop for lpitch in (lmidic self)
          for onset in (lonset self)
          for ldur in (ldur self)
          for lvel in (lvel self)
          for loffset in (loffset self)
          for lchan in (lchan self)
          for lport in (lport self)
          append  (loop for pitch in lpitch
                        for dur in ldur
                        for vel in lvel
                        for offset in loffset
                        for chan in lchan
                        for port in lport
                        collect (list  pitch (+ onset offset) dur vel chan port))
          ))

(defmethod tm-mf-info->chord-seq ((self list))
  (let* ((chords (tm-make-quanti-chords self *global-deltachords*))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      :legato 0
    )))

(defmethod tm-align-chordseq-chords ((self chord-seq))
  (align-offsets (tm-mf-info->chord-seq (tm-chord-seq->mf-info self))))


;;; aligne les accords d'un chord-seq

(defmethod! align-chords ((self chord-seq) (delta integer))
  :initvals (list (make-instance 'chord-seq) 100)
  :indoc '("a chord-seq" "an integer")
  :icon 230
  :doc "Transforms <self> so that notes falling in a small time interval are grouped into a chord.
<delta> gives the time interval in ms
If delta = nil , align-chords does not act"
  (let ((*global-deltachords* delta))
    (tm-align-chordseq-chords self)))

|#


#|
(defmethod* merger ((chs1 chord-seq) (chs2 chord-seq))
  (let* ((mf  (sort  (nconc (tm-chord-seq->mf-info chs1)  (tm-chord-seq->mf-info chs2))
                     #'< :key #'second)))
    (tm-mf-info->chord-seq mf)))
|#


        
