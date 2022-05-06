(in-package :om)


; =================== frequentiel ======================================

; ----------------------------harm-series provenant d'Esquisse------------------------------

(defun rankcalc (numer denom begin end )
  (let ((cdeb (mod begin denom)) (pas (if (< end begin) -1 1)) res)
    (for (n begin pas end) 
      (if (not (>= (mod (- n cdeb) denom) numer)) (push n res)) )
    (nreverse (remove 0 (if (and (not (null (find 1 res))) (not (null (find -1 res)))) 
                        (remove -1 res) res)))))

(defun hrmcalc1 ( listharm fond)
  (let (res)
    (dolist ( n listharm)
          (cond ( ( > n 0 ) (push (* fond  n ) res))
                ( ( < n 0 ) (push (/ fond (abs n))  res))
                ( t () ) ))
    (nreverse  res)))

(defun hrmcalc (fond listharm )
  (less-deep-mapcar #'hrmcalc1 listharm fond))

(defun sercalc (fund  numer denom begin end )
  (hrmcalc fund (rankcalc numer denom begin end) ))

(defun one-nth (fund nth type)
  (let ((res (deep-mapcar/1  'hrmcalc  fund  nth)))
    (if (equal type 'chord) (flat-once res) res)))


(defun duo-nth (fund nth type)
  (if (equal type 'chord) (flat-once (one-nth fund nth type))
      (mapcar #'(lambda (x) (flat (one-nth fund x type))) nth)))

(defun seqf-nth (fund nth type listnth)
  (cond ((not listnth) (mapcar #'(lambda (x) (one-nth x nth 2)) fund)) 
        ((equal type 'chord) (flat-once (mapcar #'(lambda (x) (duo-nth x nth  2)) fund)))
        ((equal type 'chordseq)  (mapcar #'(lambda (x) (flat (one-nth x nth type))) fund) )))


;====Removed from menu 22-11-2006==================================================
; redéfini , types changés  (= harm-series)
(om::defmethod! harm-ser ((fund t) (numer integer) 
                         (denom integer) (begin integer)
                         (end integer)
                         &optional (unit 'midic) (type 'chord))
  :initvals '(3600 1 1 1 7 'midic 'chord)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "Unit" "Type")
  :menuins '((5 (("Midics" 'midic) ("Freqs" 'freq))) (6 (("Chord" 'chord) ("ChordSeq" 'chordseq))))
  :icon 137
  :doc  "Builds the harmonic and/or sub-harmonic series on fundamental <fund>. The arguments <numer> and <denom> determine what sample (<numer>/<denom>) of the partials is taken. (e.g. 1/1 = all; 1/2 = every other; 2/5 = the first two of each group of five).

The arguments <begin> and <end> determine the lowest and highest partials generated. The fundamental is represented by '1' or '-1'. Sub-harmonics are represented by negative numbers, overtones by positive. (e.g. partial number 7 is 7 times the fundamental frequency; partial -7 is the fundamental frequency divided by 7; thus to go from the seventh undertone to the seventh overtone <begin> would equal '-7' and <end> would equal '7')

Options: The menu argument <unit> determines whether the <fund> is entered in midicents, ('midic'), or in Hertz ('freq'). If 'midic' is selected the value will be converted to frequency inside the function and then the output is reconverted to midicents. If 'freq' is selected the entry, both calculation and output are done in Hertz.

When <fund> is a list, the menu argument <type> is used to determine the format of the output. The value 'seq' returns a list of chords 
representing the partials requested for each successive fundamental. The value 'chord' returns a single chord containing all the partials for all fundamentals." 
  (let* ((fund (if (equal unit 'freq) fund (mc->f  fund)))
         (res (car-mapcar 'sercalc  fund  numer denom begin end)))
    (setq res (if (equal unit 'freq) res  (f->mc res) ))
    (if (equal type 'chord) (flat res) res)))


;====Removed from menu 22-11-2006==================================================
(defmethod! n-harm ((fund t) (nth t) &optional (unit 'midic) (type 'chord))
  :initvals '(3600 '(1 2 3 4 5) 'midic 'chord)
  :indoc '("Fundamental" "Partial-numbers" "Unit" "Type")
  :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq))) (3 (("Chord" 'chord) ("ChordSeq" 'chordseq))))
  :icon 137 
  :doc  
  "Receives a fundamental <fund>, or list of fundamentals, and returns a list of partials with rank <nth>. <nth> can be a number or a list of number. Negative numbers indicate subharmonics. Floating point numbers mean non-harmonic partials.

Options: The menu argument <unit> determines whether the <fund> is entered in midicents, ('midic') or Hertz ('freq'). If 'midic' is selected the value will be converted to frequency inside the function and then the output is reconverted to midicents. If 'freq' is selected the entry, both calculation and output are done in Hertz.

When <fund> is a list, the menu argument <type> is used to determine the format of the output. The value 'ChordSeq' returns a list of chords representing the partials requested for each successive fundamental. The value 'chord' returns a single chord containing  the merged partials for all fundamentals."

  (let* ((nth (list! nth)) (fund (list! fund))
         (listfund  (not (one-elem fund)))
         (listnth (not (atom (car nth))))
         (listchord (not (atom (car fund))))
         (fund (if (equal unit 'freq)  fund   (mc->f fund)))
         res)
    (cond  (listchord (setq res (seqf-nth fund nth type listnth)))
           ((and listfund listnth) (setq res (duo-nth fund nth type)))
           ((not listfund)  (setq res (flat-once (one-nth fund nth type))))
           (t (setq res (one-nth fund nth type))))
    (if (one-elem res) (setq res (first res)))
    (if (equal unit 'freq) res (f->mc res))))

; ----------------------------harm-series------------------------------


; revoir pb de liste de fondamentales

(defun hqm (fund n)
"harm sup ou inf (midic) de fund (midic)"
 (f->mc (hqf fund n )))

(defun hqf (fund n)
"harm sup ou inf (freq) de fund (midic)"
 (if (> n 0) (* (mc->f fund) n) (/ (mc->f fund) (abs n))))



(defun rangcalc (numer denom begin end )
  (let ((cdeb (mod begin denom)) (pas (if (< end begin) -1 1)) res)
    (for (n begin pas end) 
      (if (not (>= (mod (- n cdeb) denom) numer)) (push n res)) )
    (nreverse (remove 0 (if (and (not (null (find 1 res))) (not (null (find -1 res)))) 
                        (remove -1 res) res)))))


   
(defun hcalc (fond listharm dist% offset approx)
  (let ( (d (+ 1 (/ dist% 100)))   res )
    (dolist  ( n listharm)
      (cond ( ( > n 0 ) (push (f->mc (+ (* fond (expt n d)) offset)) res))
            ( ( < n 0 ) (push (f->mc (+ (/ fond (expt (abs n) d)) offset)) res))
            ( t () ) ))
    (if (eq approx nil) (nreverse  res) (approx-m (nreverse  res) approx))))

(defun polycalc (fund dist% numer denom begin end offset approx)
  (hcalc fund (rangcalc numer denom begin end) dist% offset approx))




; ajout methodes pour objets  11-10-04

(om::defmethod! polysp ((fund number) (dist% number) 
                    (numer integer) (denom integer) (begin integer) (end integer) 
                    &optional (offset 0) (approx nil))

  :initvals '(3600 0 1 1 1 7 0 nil)
  :indoc '("Fundamental" "Dist" "Numerator" "Denominator" "Begin" "End" "Offset" "Approx")
  :menuins '((7 (("nil" nil) ("1/2" '1/2) ("1/4" '1/4) ("1/8" '1/8))))
  :icon 137 
  :doc "tous les <numer> sur <denom> partiels de <fund> depuis <begin> à <end>
 selon algorithme : p = f * r ^ d + offset
d = 1 + dist%/100 (dist% = distorsion exprimée en %)
offset = freq shift"
  (let ((approx 
         (cond ((null approx) nil)
               ((equal approx '1/2) 2)
               ((equal approx '1/4) 4)
               ((equal approx '1/8) 8))))
(polycalc (mc->f (carlist! fund)) dist% numer denom begin end offset approx)))



(om::defmethod! polysp ((fund list) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
  
  (if (one-elem fund) (polysp (car fund) dist% numer denom begin end offset approx)
  (loop for f in fund
        collect (polysp f dist% numer denom begin end offset approx))))




(om::defmethod! polysp ((fund chord) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
 (if (one-elem (lmidic fund))
   (make-instance 'chord
    :LMidic (polysp (lmidic fund) dist% numer denom begin end offset approx)
    :Lvel (lvel fund)
    :Loffset (loffset fund)
    :Ldur (ldur fund)
    :Lchan (lchan fund)
    :lport (lport fund))

   (polysp (explosion fund) dist% numer denom begin end offset approx)))



(om::defmethod! polysp ((fund chord-seq) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
  (make-instance 'chord-seq
    :lmidic 
    (loop for c in (chords (explosion fund))
          collect (polysp c dist% numer denom begin end offset approx))
    :lonset (lonset (explosion fund))
    :legato (legato fund)))


(om::defmethod! polysp ((fund note) (dist% number) 
                    (numer integer) (denom integer) (begin integer) (end integer) 
                    &optional (offset 0) (approx nil))
  (make-instance 'chord
    :LMidic (polysp (midic fund) dist% numer denom begin end offset approx)
    :Lvel (list (vel fund))
    :Loffset (list(offset fund))
    :Ldur (list(dur fund))
    :Lchan (list(chan fund))
    :lport (list(port fund))))



;====Rename polysp as sp-gen 22-11-2006==================================================

(om::defmethod! sp-gen ((fund number) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
  :initvals '(3600 0 1 1 1 7 0 nil)
  :indoc '("Fundamental" "Dist" "Numerator" "Denominator" "Begin" "End" "Offset" "Approx")
  :menuins '((7 (("nil" nil) ("1/2" '1/2) ("1/4" '1/4) ("1/8" '1/8))))
  :icon 137 
  :doc "Returns all partials of fundamental <fund> determined by a numerator <numer> and a denominator <denom> from beginning <begin> to end <end>.

The following algorithm is used: p = f * r ^ d + offset.
Here, d = 1 + dist%/100 (dist% = distortion percentage).
In case d = 1, there is no distortion (dist% = 0) and the series is harmonic.
In case d < 1, we have a distorted (i.e. inharmonic) spectrum which is compressed.
In case of d > 1 we have a distorted (i.e. inharmonic) spectrum which is expanded.

Optional: With ‘Offset’ a frequency shift may be added to the function."
  (polysp fund dist% numer denom begin end offset approx))

(om::defmethod! sp-gen ((fund list) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
  
  (polysp fund dist% numer denom begin end offset approx))


(om::defmethod! sp-gen ((fund chord) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
 (polysp fund dist% numer denom begin end offset approx))



(om::defmethod! sp-gen ((fund chord-seq) (dist% number) 
                        (numer integer) (denom integer) (begin integer) (end integer) 
                        &optional (offset 0) (approx nil))
  (polysp fund dist% numer denom begin end offset approx))


(om::defmethod! sp-gen ((fund note) (dist% number) 
                    (numer integer) (denom integer) (begin integer) (end integer) 
                    &optional (offset 0) (approx nil))
  (polysp fund dist% numer denom begin end offset approx))

;========================================================================================


; a faire : créer des objets  quand fund = objet

(om::defmethod! nth-polysp ((fund t) (nth t )
                        (dist% number) 
                        &optional (offset 0) (approx nil))

  :initvals '(3600 '(1 2 3 4 5) 0 0 nil)
  :indoc '("Fundamental" "Ranks" "Distor" "Offset" "Approx")
  :menuins '((4 (("nil" nil) ("1/2" '1/2) ("1/4" '1/4) ("1/8" '1/8))))
  :icon 137 

  :doc "Returns the <nth> harmonic(s) of the fundamental <fund>. <fund> can be atom,
or a list any depth ; <nth> can be atom or list ;<nth> may be non integer
dist% et offset optionnels
algorithme : p = f * r ^ d + offset
d = 1 + dist%/100 (dist% = distorsion exprimée en %)
offset = freq shift"
(let* ((approx 
         (cond ((null approx) nil)
               ((equal approx '1/2) 2)
               ((equal approx '1/4) 4)
               ((equal approx '1/8) 8)))
       (res (deep-mapcar/1   'hcalc (mc->f (carlist! fund)) (list! nth) dist% offset approx)))
  (if (one-elem res) (first res) res)))


(om::defmethod! nth-polysp ((fund chord) (nth t )
                        (dist% number) 
                        &optional (offset 0) (approx nil))
(make-instance 'chord
    :LMidic (flat (nth-polysp (lmidic fund) nth dist% offset approx))
    :Lvel (lvel fund)
    :Loffset (loffset fund)
    :Ldur (ldur fund)
    :Lchan (lchan fund)
    :lport (lport fund)))

(om::defmethod! nth-polysp ((fund chord-seq) (nth t )
                            (dist% number) 
                            &optional (offset 0) (approx nil))
  (make-instance 'chord-seq
    :lmidic 
    (loop for c in (chords  fund)
          collect (flat (nth-polysp (lmidic c) nth dist% offset approx)))
    :lonset  (lonset fund)
    :legato (legato fund)))


;====Rename nth-polysp as N-SP-GEN 22-11-2006==================================================

(om::defmethod! n-sp-gen ((fund t) (nth t )
                        (dist% number) 
                        &optional (offset 0) (approx nil))

  :initvals '(3600 '(1 2 3 4 5) 0 0 nil)
  :indoc '("Fundamental" "Ranks" "Distor" "Offset" "Approx")
  :menuins '((4 (("nil" nil) ("1/2" '1/2) ("1/4" '1/4) ("1/8" '1/8))))
  :icon 137 

  :doc "Returns the <nth> partial(s) of fundamental <fund>. <fund> can be an atom or a list any depth ; <nth> can be an atom or a list ; <nth> may be a non-integer. ‘d’ allows to input a distortion factor immediately.

The following algorithm is used: p = f * r ^ d + offset.
Here, d = 1 + dist%/100 (dist% = distortion percentage).
In case d = 1, there is no distortion (dist% = 0) and the series is harmonic.
In case d < 1, we have a distorted (i.e. inharmonic) spectrum which is compressed.
In case of d > 1 we have a distorted (i.e. inharmonic) spectrum which is expanded.

Optional: With ‘offset’ a frequency shift may be added to the function. ‘approx’ allows to specify the desired approximation of the pitches to the nearest 1/2 th, 1/4 th, or 1/8th tone."

  (nth-polysp fund nth dist% offset approx))


(om::defmethod! n-sp-gen ((fund chord) (nth t )
                        (dist% number) 
                        &optional (offset 0) (approx nil))
  (nth-polysp fund nth dist% offset approx))



(om::defmethod! n-sp-gen ((fund chord-seq) (nth t )
                            (dist% number) 
                            &optional (offset 0) (approx nil))
  (nth-polysp fund nth dist% offset approx))

;========================================================================================







; --------------------- modulation de fréquence ---------------------



(defun fmnthtm (p m lindex output)
  (let ((res (if (equal output 'exclus) () (list p))))
  (while  lindex 
    (let ((i (pop lindex)))
      (push (+ p (* i m)) res)
      (push (- p (* i m)) res)))
  (reverse res)))



(defmethod! fm/origin ((diff number) (add number))
  :initvals '(7200 4800)
  :indoc '("Differential" "Additional")
  :icon 136
  :doc "rend liste (porteuse  modulante) en fct différentiel et
additionnel (d'indice 1) "
(let ((d (mc->f (car! diff))) (a (mc->f (car! add))) port modul)
  ;(if  (> d a) (setq a (mc->f (car! diff)) d (mc->f (car! add))))
  (setq port (/ (+ a d) 2) modul (abs (/ (- a d) 2)))
  (format t "porteuse = ~S ; modulante = ~S ; ratio = ~S  /   "
          (om-round port 3) (om-round modul 3) (om-round (/ modul port) 3))
  (f->mc (list port modul))))


;====Rename fm/origin as fm-origin 22-11-2006==================================================

(om::defmethod! fm-origin ((diff number) (add number))
  :initvals '(7200 4800)
  :indoc '("Differential" "Additional")
  :icon 136
  :doc "Returns a list (carrier, modulator) after input of given difference and addition tones (with i=1)."

  (fm/origin diff add))

;==============================================================================================
; verifier pour carrier = liste (les options chord - chordseq ne marchent pas)

;====<<TO DO: Merge fm/midic and fm/freq>>==================================================

(defmethod! fm/freq ((fcarrier t) (fmod t) (index t)
                  (format symbol) &optional (output t))
  :initvals '(261 440 5 'Chord 'inclus)
  :indoc '("Carrier" "Modulator" "Index" "Format" "Output")                                 
  :menuins '((3 (("Chord" 'Chord) ("Chordseq" 'Chordseq) )) (4 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 137
  :doc  "Input  = freq ; output = object
   fcarrier fmod and index may be lists"

(let* (ll (x (one-elem fcarrier)) (fcarrier (list! fcarrier)) (fmod (list! fmod))
          (index (if (atom index) (arithm-ser  1  index 1) index ))
     ;  (lindex (append '(0) (list-fill  index  (* 2 (length index))))) 
       res)
    (while fcarrier
      (let ((a (pop fcarrier)))
        (push (unique (flat (mapcar #'(lambda (x) (f->mc (fmnthtm a x index output))) fmod))) ll) ))
    
    (if (equal format 'Chord)
      (if  x 
        (setq res (make-instance 'chord 
                    :lmidic (flat (nreverse ll) )))
        (dolist (n  ll)
          (push (make-instance 'chord 
                    :lmidic n  ) res)))
      (setq res (if  x (flat (nreverse ll)) (nreverse ll))))
    res))



(om::defmethod! fm/midic ((mcarrier t) (mmod t) (index t )
                  (format symbol ) &optional (output t))
  :initvals '(6000 6600 5 'Chord 'inclus)
  :indoc '("Carrier" "Modulator" "Index" "Format" "Output")                                 
  :menuins '((3 (("Chord" 'Chord) ("Chordseq" 'Chordseq) )) (4 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 137
  :doc   "MF en midics, si index=atome, MF classique, si index=liste, MF calculée 
seulement pour les valeurs de cette liste"
     (fm/freq (mc->f mcarrier) (mc->f mmod) index format output))



(om::defmethod! fm/ratio ((carrier t) (ratio t) (index t )
                   (format symbol) &optional (output t)) 

  :initvals '(6000 1.42 5 'Chord)
  :indoc '("Carrier" "Modulator" "Index" "Format" "Output")                                 
  :menuins '((3 (("Chord" 'Chord) ("Chordseq" 'Chordseq) )) (4 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 136
  :doc  "porteuse=midic sortie=objet ou liste"

  (fm/freq (mc->f carrier) (om* ratio (mc->f carrier)) index format output))



;====Rename fm/ratio as FM-ratio 22-11-2006==================================================

(om::defmethod! fm-ratio ((carrier t) (ratio t) (index t )
                   (format symbol) &optional (output t)) 

  :initvals '(6000 1.42 5 'Chord)
  :indoc '("Carrier" "Modulator" "Index" "Format" "Output")                                 
  :menuins '((3 (("Chord" 'Chord) ("Chordseq" 'Chordseq) )) (4 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 136
  :doc  "porteuse=midic sortie=objet ou liste"

  (fm/ratio carrier ratio index format output))

;=============================================================================================




(om::defmethod! fm/fan ((carrier number) (ratio number) (index t)
                    (del1 number) (deltot number) (durs t) (dyns t) &optional (output t))

  :initvals '(6000 1.42 5 200 2000 200 80 'inclus)
  :indoc '("Carrier" "Ratio" "Index" "First delay" "Total delay" "Durs" "Dyns" "Output") 
  :menuins '((7 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 136                                
  :doc  "deltot : temps entre index 0 et index <index>
 del1 : temps entre index 0 et index 1"

(let* ((index (if (atom index) (arithm-ser 1 index 1 ) index))
       (lg (length index))
       (hauteurs (f->mc (fmnthtm (mc->f carrier) (om* ratio (mc->f carrier)) index output)))
      ; (lindex (append '(0) (list-fill  index  (* 2 lg))))

       (offs (if (> lg 1) 
               (x-append 0
                     (list-fill (om-round 
                                 (power/3 (arithm-ser 1 lg 1)  0 0 1 del1 lg deltot )  1) 
                                (* 2 lg)))
               (list 0 deltot deltot))))
  
  (make-instance 'chord 
                    :lmidic hauteurs
                    :lvel (list! dyns)
                    :ldur (list! durs) 
                    :loffset (om-round offs) )))


;====Rename fm/fan as fm-arp 22-11-2006==================================================

(defmethod! fm-arp ((carrier number) (ratio number) (index t)
                    (del1 number) (deltot number) (durs t) (dyns t) &optional (output t))

  :initvals '(6000 1.42 5 200 2000 200 80 'inclus)
  :indoc '("Carrier" "Ratio" "Index" "First delay" "Total delay" "Durs" "Dyns" "Output") 
  :menuins '((7 (("Inclus" 'inclus) ("Exclus" 'exclus) )))
  :icon 136                                
  :doc
"Returns an arpeggio of the frequency modulation sound. The component of the sound with the higher index starts later than those with a lower indices.

deltot: time between index 0 and index <index>.
del1: time between index 0 and index 1"

  (fm/fan carrier ratio index del1 deltot durs dyns output))

;=============================================================================================









;======================= from Esquisse  ==================================

(om::defmethod! freq-mod ((carrier number) (modul number) (index number) (unit symbol) (type symbol))
   :initvals '(3600 4000 1 'midic 'chord)
   :indoc '("Carrier" "Moduler" "Index" "Unit" "Type")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq)))(4 (("Chord" 'chord) ("ChordSeq" 'chordseq))))
   :icon 136 
   :doc  
   "Computes a FM spectrum from <carrier>, <modul> and <index>.
<carrier> and <modul> may be expressed in midics (the default) or in freqs. In that case
the menu <unit> must be set to 'Freq'.
<index>, the modulation index is a positive integer between 1 and 25.
Outputs a chord object.
<carrier>, <modul> and <index> may also be lists. In that case,  the 1st item in <carrier>
is modulated by the 1st item in <modul>,  with the 1st index in <index>, and so on.
The results are then merged into a chord object.
When using lists, you may also have the result as a series of chords in a 
chord-seq object by selecting 'chordseq' in the menu <type>"
   
   (when (eq unit 'midic)
     (setf carrier (mc->f carrier) modul (mc->f modul)))
   (let* ((spec (fmspec carrier modul index))
          (vel (mapcar #'(lambda (x) (round (* (/ 127 3.0) (if (<= (cdr x) 0.0) 0 (log (cdr x) 10))))) spec))
          (spec (band-filter (mapcar #'car spec) '((15.0 20000.0)) 'pass)) )
     (make-instance 'chord
       :lmidic (f->mc spec)
       :lvel vel) ))


(defmethod! freq-mod ((carrier list) (modul list) (index list) (unit symbol) (type symbol))
  (let ((fmlist (loop while (or carrier modul index)
                      for car = (pop carrier) then (if carrier (pop carrier) car)
                      for mod = (pop modul) then (if modul (pop modul) mod)
                      for ind = (pop index) then (if index (pop index) ind)
                      collect (freq-mod car mod ind unit type))))
    (if (eq type 'chordseq)
      (make-instance 'chord-seq :lmidic fmlist)
      (make-instance 'chord :lmidic (flat (mapcar 'lmidic fmlist)) :lvel (flat (mapcar 'lvel fmlist))))))

(defmethod! freq-mod ((carrier t) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod (list! carrier) (list! modul) (list! index) unit type))

(defmethod! freq-mod ((carrier chord) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod (lmidic carrier) modul index 'midic type))


;====Rename freq-mod as fmo 22-11-2006==================================================

(defmethod! fmo ((carrier number) (modul number) (index number) (unit symbol) (type symbol))
   :initvals '(3600 4000 1 'midic 'chord)
   :indoc '("Carrier" "Moduler" "Index" "Unit" "Type")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq)))(4 (("Chord" 'chord) ("ChordSeq" 'chordseq))))
   :icon 136 
   :doc 
   "Computes a frequency modulation spectrum, returning a chord object. The carrier frequency C <carrier> is modulated by the modulating frequency M <modul> with modulation index I <index>.

<carrier> and <modul> may be expressed in midics (the default) or in freqs. In that case the menu <unit> must be set to 'Freq'. <index>, the modulation index, is a positive integer between 1 and 25.

The inputs <carrier>, <modul> and <index> may also be lists. In that case, the nth element in <carrier> is modulated by the nth element in <modul> using the nth element in <index>, and so on. The results are then merged into a chord object (by default) or a chord-seq object by selecting 'chordseq' in the menu <type>.

A short explanation of FM (Curtis Roads, the computer music tutorial, MIT press, Massachusetts, p. 227-230): In case the inputs are atoms, we have Simple FM or Chowning FM (Chowning 1973). FM between two sinusoids generates a series of sidebands around C. Each sideband is located at a distance equal to a multiple of M. When the ratio between C and M (the C:M ratio) is a simple integer ratio such as, for example, 3:2 or 4:1, FM generates harmonic spectra. This means that the sidebands are integer multiples of both M and C. When C:M is not a simple integer ratio, FM creates inharmonic spectra. The number of sidebands the bandwidth) of the FM spectrum is controlled by the modulation index I, etc."

   (freq-mod carrier modul index unit type))

(defmethod! fmo ((carrier list) (modul list) (index list) (unit symbol) (type symbol))
  (let ((fmlist (loop while (or carrier modul index)
                      for car = (pop carrier) then (if carrier (pop carrier) car)
                      for mod = (pop modul) then (if modul (pop modul) mod)
                      for ind = (pop index) then (if index (pop index) ind)
                      collect (freq-mod car mod ind unit type))))
    (if (eq type 'chordseq)
      (make-instance 'chord-seq :lmidic fmlist)
      (make-instance 'chord :lmidic (flat (mapcar 'lmidic fmlist)) :lvel (flat (mapcar 'lvel fmlist))))))

(defmethod! fmo ((carrier t) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod carrier modul index unit type))

(defmethod! fmo ((carrier chord) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod carrier modul index unit type))

;======================================================================================



; ------ freq modulation fcts -------------

(defvar *maxorder* 25)
(defvar *bessel*
      (make-array '(26 25)
                  :initial-contents
      '(
        ( 1000 765 223 -260 -397 -177 150 300 171 -90 -245 -171 47 206 171 -14 -174 -169 -13 
           146 167 36 -120 -162 -56)
        ( 0 440 576 339 -66 -327 -276 -4 234 245 43 -176 -223 -70 133 205 90 -97 -187 -105 66 
          171 117 -39 -154)
        ( 0 114 352 436 364 46 -242 -301 -112 144 254 139 -84 -217 -152 41 186 158 -7 -157 -160 
           -20 131 158 43)
        ( 0 19 128 309 430 364 114 -167 -291 -180 58 227 195 3 -176 -194 -43 134 186 72 -98 -174 
           -93 67 161)
        ( 0 2 33 132 281 391 357 157 -105 -265 -219 -15 182 219 76 -119 -202 -110 69 180  130 -29 
           -156 -141 -3)
        ( 0 0 7 43 132 261 362 347 185 -55 -234 -238 -73 131 220 130 -57 -187 -155 3 151 163 36 
           -116 -162)
        ( 0 0 1 11 49 131 245 339 337 204 -14 -201 -243 -118 81 206 166 0 -155 -178 -55 107 173 90 
           -64)
        (0 0 0 2 15 53 129 233 320 327 216 18 -170 -240 -150 34 182 187 51 -116 -184 -102 58 163 130)
        (0 0 0 0 4 18 56 127 223 305 317 224 45 -141 -231 -173 -7 153 195 92 -73 -175 -136 8 140)
        (0 0 0 0 0 5 21 58 126 214 291 308 230 66 -114 -220 -189 -42 122 194 125 -31 -157 -157 -36)
        (0 0 0 0 0 1 6 23 60 124 207 280 300 233 85 -90 -206 -199 -73 91 186 148 7 -132 -167)
        (0 0 0 0 0 0 2 8 25 62 123 201 270 292 235 99 -68 -191 -204 -98 61 173 164 42 -103)
        (0 0 0 0 0 0 0 2 9 27 63 121 195 261 285 236 112 -48 -176 -205 -118 32 156 173 72)
        (0 0 0 0 0 0 0 0 3 10 28 64 120 190 253 278 236 122 -30 -161 -204 -135 6 137 176)
        (0 0 0 0 0 0 0 0 1 3 11 30 65 118 185 246 272 236 131 -15 -146 -200 -148 -17 118)
        (0 0 0 0 0 0 0 0 0 1 4 13 31 65 117 181 239 266 235 138 0 -132 -195 -158 -38)
        (0 0 0 0 0 0 0 0 0 0 1 5 13 32 66 116 177 234 261 234 145 12 -118 -189 -166)
        (0 0 0 0 0 0 0 0 0 0 0 1 5 14 33 66 114 173 228 255 233 150 23 -105 -183)
        (0 0 0 0 0 0 0 0 0 0 0 0 2 6 15 34 66 113 170 223 251 231 154 34 -93)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 2 6 16 35 67 112 167 218 246 229 158 43)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 7 17 36 67 111 164 214 242 228 161)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 7 18 36 67 110 162 210 238 226)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 18 37 67 109 159 206 234)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 19 38 67 108 157 203)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 9 19 38 67 107 155)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 4 9 20 39 67 106)
      )
      )
      )

(defun fix (x) (truncate x))
(defun ceil (x) (ceiling x))
(defun fixp (x) (integerp x))
(defun add1 (x) (1+ x))

(defun fmSpec (c m imod &optional order)
    (let ((spec) s p  MI)
         (setq MI imod)
         (if (floatp imod) (setq imod (ceil imod)) (setq imod (fix imod)))
         (if (null order)
             ;(setq order (car order))
             (setq order (add1 imod)) )
         (setq order (min order *maxorder*))
         (setq spec `((, c . 0 )))
         (for (i 1 1 order)
              (newl spec (cons (- c (* i m)) (- i)))
              (setq  spec (nconc spec (list (cons (+ c (* i m)) i))))
              (when (and (null p) (< (caar spec) 0))
                    (setq p spec)))
         (setq s spec)
         (while s
                ;(when (and (null p) (>= (caar s) 0))
                ;      (setq p q))
                (cond 
                      ( (< (cdar s) 0)
                        (if (oddp (cdar s))
                            (rplacd (car s) (- (bessel MI (abs (cdar s)))))
                            (rplacd (car s) (bessel MI (abs (cdar s)))))
                        (when (< (caar s) 0)
                              (rplaca (car s) (- (caar s)))
                              (rplacd (car s) (- (cdar s)))))
                      ( t
                        (rplacd (car s) (bessel MI (cdar s)))))
                ;(setq q s)
                (nextl s))
         (setq spec
               (if (not p)
                    spec
                    (fmMerge (cdr p) 
                             (progn (rplacd p ()) (nreverse spec)))))
         (mapc #'(lambda (comp)
                       (rplacd comp (abs (cdr comp))))
               spec)
         (when (<= (caar spec) 0) (nextl spec))
          (fmNormalize spec)
         spec
))


(defun fmNormalize (spec)
    (let ((etot 0) ratio)
         (mapc #'(lambda (x) (setf  etot (+ etot (cdr x))))
               spec)
         (setq ratio (/ 1000.0 etot))
         (mapc #'(lambda (x)
                  (rplacd x (fix (* (cdr x) ratio))))
               spec)
        spec))


 
(defun bessel (imod i)
    (if (fixp imod)
        (aref *bessel* i imod)
        (let ((i1 (aref *bessel* i (fix imod))) (i2 (aref *bessel* i (ceil imod))))
             (fix (+ i1 (* (- imod (fix imod)) (- i2 i1)))))))

(defun fmMerge   (f1 f2)
    (let ((r (list ())))
         (fmMerge2 r f1 f2)
         (cdr r)))


(defun fmMerge2 (r f1 f2)
    (cond 
          ((null f1) (rplacd r f2))
          ((null f2) (rplacd r f1))
          ((< (caar f1) (caar f2))
           (rplacd r f1)
           (fmMerge2 f1 (cdr f1) f2))
          ((= (caar f1) (caar f2))
           (rplaca f1 (cons (caar f1) (+ (cdar f1) (cdar f2))))
           (rplacd r f1)
           (fmMerge2 f1 (cdr f1) (cdr f2)))
          (t (rplacd r f2)
             (fmMerge2 f2 f1 (cdr f2)))))


(defun fm (c m i)
  (let ((spec (fmspec c m i)))
    (cons (mapcar #'car spec)
          (mapcar #'(lambda (x) (round (* (/ 127 3.0) (log (cdr x) 10)))) spec))))




; ----------------------ring modulation---------------------------

; from Esquisse (ring-mod) - optional ajouté -  type et unit intervertis

(defun ring/freq (freqs1 freqs2 )
"Rend une liste de listes de fréquences contenant la modulation en anneau 
de chaque fréquence de la liste <freqs1> par la liste <freqs2>"
  (let* (ll (freqs1 (list! freqs1)) (freqs2 (list! freqs2))
         (x (one-elem freqs1)))
    (while freqs1
      (let ((a (pop freqs1)))
        (push (append (om+ a freqs2) (om- a freqs2)) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))

(defmethod! ring-modulation ((ch1 number) (ch2 number) &optional (type 'chord) (unit 'midic) )
   :initvals '(6000 6200  'chord 'midic)
   :indoc '("Chord" "Chord"  "Type" "Unit")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq))) (2 (("Chord" 'chord) ("ChordSeq" 'chordseq))) )
   :icon 137 
   :doc  
   "Simulates the ring modulation of each note of <ch1> by all the notes of 
<ch2>. The frequency of each note of <ch2> is added to and subtracted 
from the frequency of each note of <ch1>; thus, all the possible 
additive and subtractive combinations are produced.

<ch1> and <ch2> may be midics, list of midics, list of lists of midics or chord objects.

The optional argument <unit> determines whether <ch1> and <ch2> are 
entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is 
selected the values will be converted to frequencies inside the function 
and then the output is reconverted to midicents. If 'freq' is selected the 
entries, calculations and output are all in hertz. (note: Ring-modulation 
can produce negative frequencies; conversion to midicents will 
automatically reflect these notes back into the positive domain.)

When <ch1> contains multiple notes, the optional argument <type> is used 
to determine the format of the output. The value 'seq' returns a list of 
chords representing the modulation of each successive note of <ch1> by 
all the notes of <ch2>. The value 'chord' returns a single chord 
containing all the notes of all the modulations.

The output is always list of midics or list of list of midics.
<ch1> and <ch2> may be chord objects in which case the unit is set to 'midic internally.

Certain combinations may produce too many levels of parentheses: use <flat-once> after <ring-modulation>
"
   

   (let ((res (if (eq unit 'freq) 
                (ring/freq  ch1  ch2)
                (f->mc (ring/freq (mc->f ch1) (mc->f ch2))))))
     (setq res (push (x-append ch1 ch2) res))
     (setq res (band-filter res '((100 12000)) 'pass))
     (if (or (eq type 'chord) (one-elem ch1) ) (flat res)  res  )))


(defmethod! ring-modulation ((ch1 t) (ch2 t) &optional (type nil) (unit nil))

  (let ((ring  (remove-dup
               (loop for note in (list! ch1)
                     collect (loop for mod in (list! ch2) append (ring-modulation note mod  type unit )))
               'eq
               2)))
    (if (eq type 'chord)
      (remove-dup (flat ring) 'eq 1)
          ring ))   )


(defmethod! ring-modulation ((ch1 CHORD) (ch2 CHORD) &optional (type nil) (unit nil))
   (ring-modulation (lmidic ch1) (lmidic ch2) type 'midic ))

;====Rename ring-modulation as rmo 22-11-2006==================================================

(defmethod! rmo ((ch1 number) (ch2 number) &optional (type 'chord) (unit 'midic))
   :initvals '(6000 6200  'chord 'midic)
   :indoc '("Chord" "Chord"  "Type" "Unit")
   :menuins '((2 (("Chord" 'chord) ("ChordSeq" 'chordseq))) (3 (("Midics" 'midic) ("Freqs" 'freq))))
   :icon 137
   :doc
   "Simulates the ring modulation of each note of <ch1> by all the notes of  <ch2>. The frequency of each note of <ch2> is added to and subtracted from the frequency of each note of <ch1>; thus, all the possible additive and subtractive combinations are produced.

<ch1> and <ch2> may be midics, list of midics, list of lists of midics or chord objects.

The optional argument <unit> determines whether <ch1> and <ch2> are entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is 
selected the values will be converted to frequencies inside the function and then the output is reconverted to midicents. If 'freq' is selected the entries, calculations and output are all in hertz. (note: Ring-modulation can produce negative frequencies; conversion to midicents will automatically reflect these notes back into the positive domain.)

When <ch1> contains multiple notes, the optional argument <type> is used to determine the format of the output. The value 'seq' returns a list of chords representing the modulation of each successive note of <ch1> by all the notes of <ch2>. The value 'chord' returns a single chord containing all the notes of all the modulations.

The output is always list of midics or list of list of midics. <ch1> and <ch2> may be chord objects in which case the unit is set to 'midic internally.

Certain combinations may produce too many levels of parentheses: In that case use <flat-once> after <ring-modulation>."
   (ring-modulation ch1 ch2 type unit))

(defmethod! rmo ((ch1 t) (ch2 t) &optional (type nil) (unit nil))
  (ring-modulation ch1 ch2 type unit))


(defmethod! rmo ((ch1 CHORD) (ch2 CHORD) &optional (type 'chord) (unit 'midic))
   ; should compute the resulting velocities sometime
   (case type
     (chord (mki 'chord :lmidic (ring-modulation ch1 ch2 type unit)))
     (chordseq (loop for chord in (ring-modulation ch1 ch2 type unit)
                     collect (mki 'chord :lmidic chord)))))



(defmethod! rmo ((s1 chord) (s2 chord-seq)  &optional (type 'chord) (unit 'midic) )
  (mki 'chord-seq 
       :lmidic
       (flat-once (loop for chord in (chords s2)
                        collect (rmo s1 chord type unit)))))

(defmethod! rmo ((s1 chord-seq) (s2 chord)  &optional (type 'chord) (unit 'midic) )
  (mki 'chord-seq 
       :lmidic
       (flat-once (loop for chord in (chords s1)
                        collect (rmo chord s2  type unit)))))



(defmethod! rmo ((s1 chord-seq) (s2 chord-seq)  &optional (type 'chord) (unit 'midic) )
  (mki 'multi-seq 
       :chord-seqs
       (loop for chord1 in (chords s1)
             collect 
             (mki 'chord-seq 
                  :lmidic
                  (flat-once (loop for chord2 in (chords s2)
                                   collect (rmo chord1 chord2  type unit)))))))

  

;==============================================================================================


(defun ringlist (fonda fondb hqa hqb )
  (let ((lfonda (om* fonda hqa)) (lfondb (list!(om* fondb hqb))))
    (flat-once (mapcar #'(lambda (x) (ring-mod lfonda x 2 1 1)) lfondb))))

(defun ringharm/f/struc (fond1  fond2  hqa  hqb )
""
(setq fond1 (car! fond1) fond2 (car! fond2))
(if (and (atom hqa) (atom hqb)) 
  (let ( (mx (max hqa hqb)) (res ()) (resinter ()) )
    (for (m 1 1 mx)
      (for (ia 1 1 (min m hqa))
        (for (ib 1 1 (min m hqb))   
          (if (and (< ia m) (< ib m)) ()
              (progn (push (+ (* ia fond1) (* ib fond2)) resinter)
                     (if (/= (- (* ia fond1) (* ib fond2)) 0) 
                     (push  (- (* ia fond1) (* ib fond2)) resinter))))
          ))
      (push (reverse  resinter) res)
      (setq resinter () )) 
   (reverse res))
  (ringlist fond1 fond2 hqa hqb)))





(defmethod! ring-harm ((funda number) (fundb number) 
                   (hqa t) (hqb t)
                   &optional  (type nil) (output nil) (unit nil))
  :initvals '(4800 5400 2 3  'chord 'excluded 'midic)
  :indoc '("Fundam a" "Fundam b" "Harm a" "Harm b" "Type" "Output" "Unit" )
  :menuins '((4 (("chord" 'chord) ("chord list" 'chlist)))
            (5 (("excluded" 'excluded) ("included" 'included)))
            (6 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137 
  :doc  "Simulates the ring-modulation between the harmonic series (see box 
'harm-series') built on <funda> and the harmonic series on <fundb>. The 
arguments <hqa> and <hqb> determine the number of partials present for 
each fundamental. The frequencies of each partial of the harmonic series 
on <funda> is added to and subtracted from the frequency of each partial 
of the harmonic series on <fundb>; thus, all the possible additive and 
subtractive combinations are produced.

If the arguments <hqa> or <hqb> are a list, rather then including all the 
partials up to and including the number given: only the listed partials 
for both fundamentals will included in the calculations.   

The optional argument <unit> determines whether <funda> and <fundb> are 
given in midicents, ('midic'), or in hertz ('freq'). If 'freq' is selected  
entries and output are all in hertz.

The optional argument <type> is used to determine the format of the 
output. The value 'seq' returns a list of chords in which each successive 
chord represents the notes involving the next partial or partials. 
Thus the first chord contains: funda ± fundb; the second: 2*funda ± fundb,
funda ± 2*fundb and 2*funda ± 2*fundb; etc. The value 'chord' returns a 
single chord containing all the notes of all the combinations and 
differences.

The optional argument <output> determines whether the notes <funda> and
<fundb> are included ('inclu') or excluded ('exclu') from the output list 
or lists."

(let ((res (if (equal unit 'hz) (ringharm/f/struc  funda fundb hqa hqb)
    (f->mc (ringharm/f/struc (mc->f funda) (mc->f fundb) hqa hqb)))))
  (if (eq output 'included) (setq res (push (list funda fundb) res)) res)
  (if (eq type 'chord) (flat res) res)))



;====Rename ring-harm as rm-gen 22-11-2006=================================================

(defmethod! rm-gen ((funda number) (fundb number) 
                   (hqa t) (hqb t)
                   &optional  (type nil) (output nil) (unit nil))
  :initvals '(4800 5400 2 3  'chord 'excluded 'midic)
  :indoc '("Fundam a" "Fundam b" "Harm a" "Harm b" "Type" "Output" "Unit" )
  :menuins '((4 (("chord" 'chord) ("chord list" 'chlist)))
            (5 (("excluded" 'excluded) ("included" 'included)))
            (6 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137 
  :doc  "Simulates the ring-modulation between the harmonic series on <funda> and the harmonic series on <fundb>. The arguments <hqa> and <hqb> determine the number of partials present for each fundamental. The frequencies of each partial of the harmonic series on <funda> is added to and subtracted from the frequency of each partial of the harmonic series on <fundb>; thus, all possible addition and difference tones are calculated.

If the arguments <hqa> or <hqb> are lists, only the listed partials will be included in the calculations.   

The optional argument <unit> determines whether <funda> and <fundb> are given in midicents, ('midic'), or in hertz ('freq'). If 'freq' is selected entries and output are all in hertz.

The optional argument <type> is used to determine the format of the output. The value 'seq' returns a list of chords in which each successive chord represents the notes involving the next partial or partials. Thus the first chord contains: funda ± fundb; the second: 2*funda ± fundb, funda ± 2*fundb and 2*funda ± 2*fundb; etc. The value 'chord' returns a single chord containing all the notes of all the combinations and differences.

The optional argument <output> determines whether the notes <funda> and <fundb> are included ('inclu') or excluded ('exclu') from the output list or lists."

  (ring-harm funda fundb hqa hqb type output unit))

;==========================================================================================


(defun +- (a b)
  (list (+ a b) (- a b)))

(defun +&- (a b)
  (append (om+ a b) (om- a b)))

(defmethod! intermod ((accord list) (resul symbol ) (approx integer))

  :initvals '('(6000 6600)  'ring 4)
  :indoc '("List Midics" "Calcul" "Approx" )
  :menuins '((1 (("add" 'add) ("diff" 'diff) ("ring" 'ring))))
  :icon 136 
  :doc   "modulations entre les sons de l'accord, diff, add ou les deux (ring)
Toutes les combinaisons 2 à 2 sont envisagées.
Les répétitions sont enlevées, en fct de approx"

  (let ((couples (posn-match  (mc->f accord)
                              (combinaisons (arithm-ser 0  (1- (length accord)) 1))))
        (fct (case resul (add  '+) (diff  '-) (ring  '+-)))
        res)
    (dolist (c couples)
      (push (apply fct c) res))
    (unique-notes  (flat (f->mc res)) approx)))

;====Rename intermod as rm-intra 22-11-2006=================================================

(defmethod! rm-intra ((accord list) (resul symbol ) (approx integer))

  :initvals '('(6000 6600)  'ring 4)
  :indoc '("List Midics" "Calcul" "Approx" )
  :menuins '((1 (("add" 'add) ("diff" 'diff) ("ring" 'ring))))
  :icon 136 
  :doc   "Returns a choice of difference <diff> tones, addition <add> tones or both <ring> within one chord. All possible combinations are calculated. Repetitions are omitted after approximation to the nearest 1/2, 1/4th or 1/8th tone."

  (intermod accord resul approx))


;============================================================================================


(om::defmethod! resultants ((ch1 list) (ch2 list ) (resul symbol ) (approx integer))

  :initvals '('(6000 6600) '(4700 5300)  'ring 4)
  :indoc '("accord1" "accord2" "Calcul" "Approx" )
  :menuins '((2 (("add" 'add) ("diff" 'diff)  ("ring" 'ring))))
  :icon 136 
  :doc  
"sons resultants additionnels ou différentiels entre les deux accords,
sans répétition (en fct de approx) "
  (let (ll (freqs1 (mc->f (list! ch1))) (freqs2 (mc->f (list! ch2)))
            (fct (case resul (add  'om+) (diff  'om-) (ring  '+&-))))
    (while freqs1
      (let ((a (pop freqs1)))
        (push  (funcall fct a freqs2)  ll) ))
    (f->mc ll)
    (unique-notes  (flat (f->mc ll)) approx)))


;====Rename resultants as rm-approx 22-11-2006=================================================
;====<<STILL TO DO look if NIL for approx leaves all duplicates>>==============================

(om::defmethod! rm-approx ((ch1 list) (ch2 list ) (resul symbol) (approx integer))

  :initvals '('(6000 6600) '(4700 5300)  'ring 4)
  :indoc '("accord1" "accord2" "Calcul" "Approx" )
  :menuins '((2 (("add" 'add) ("diff" 'diff)  ("ring" 'ring))))
  :icon 136 
  :doc  
"Returns all possible difference tones and addition tones between two chords. Repetitions are omitted after approximation to the nearest 1/2, 1/4th or 1/8th tone."

  (x-diff (resultants ch1 ch2 resul approx) '(-1272300)))

;============================================================================================



(om::defmethod! ch-rm-sine ((object chord) (sine t) (%amp t) (type t)
(mode  t) (inclus t)
                           &optional   (channel nil) (port nil))


 :initvals '(nil 440 100 'freq  'RM 'inclus nil nil  )
 :menuins '((3 (("freq" 'freq) ("midic" 'midic))) (4 (("add" 'add)
                                                      ("diff" 'diff) ("RM" 'RM))) 
            (5 (("inclus" 'inclus) ("exclus" 'exclus))))
  :icon 137
  :doc  "Simulation of the ring modulation of an object (chord, chord-seq or multi-seq) by a sine-wave.
%amp : scales amplitude of the resulting sounds (according to a given percentage of the original amplitudes).
Choices : input of sine wave tone either as frequency <freq> or in midicents <midic>; computation of the additional or the differential tone or both (RM); initial object can be included or not.
Options : it is possible to asign a different channel and port to the resulting sounds.
In order to calculate ring modulation of the harmonics of the object, use 'mixtur' before ch-RM-sine."

(let* ((sine (list! (if (eq type 'freq) sine (mc->f sine))))
        (nbsine (length sine))
   (lcanal (if (atom channel) (create-list nbsine channel) channel))
   (lport  (if (atom port) (create-list nbsine port) port))
   (lamp (if (atom %amp) (create-list nbsine %amp) %amp ))

   (resultchord (loop for s in sine  
                      for a in lamp
                      for c in lcanal
                      for p in lport
                      collect (simpleRM object s a mode c p) ))

   (finalresult (if (one-elem resultchord) (first resultchord) (mixer
                                                                resultchord nil))))
  
(if (eq inclus 'inclus) (mixer object finalresult) finalresult)))


(defun simpleRM (object sine %amp mode channel port   )
(print
 (if (eq mode 'RM) (mixer (simpleresul object  sine %amp 'add channel port) 
                          (simpleresul object  sine %amp 'diff channel port))
   (simpleresul object  sine %amp mode channel port))))


(defun simpleresul (object sine %amp mode channel port)
  (let* ((gener (mc->f (lmidic object))))
    (mki 'chord
         :LMidic (f->mc (if (eq mode 'add )  (om+ gener sine) (om- gener sine)))
         :Ldur  (ldur object)
         :LOffset (loffset object) 
         :Lchan   (if (null channel) (lchan object )  (list! channel))
         :Lvel   (om// (om* (lvel object ) %amp)  100)
         ;    :Lport    (if (null port) (lport object ) (list! port)))) toujours ce pb avec Lport...
         :Lport  (if (null port) (lport object ) (create-list (ch-length object) port))
         )))

(om::defmethod! ch-rm-sine ((object chord-seq) (sine t) (%amp t) (type t) (mode  t) (inclus t)
                           &optional (channel nil) (port nil))
                (mki 'chord-seq
                     :lmidic (loop for ch in (chords object)
                                   collect (ch-RM-sine ch sine %amp type mode inclus channel port))
                     :lonset (lonset object)))

(om::defmethod! ch-RM-sine ((object multi-seq) (sine t) (%amp t) (type t) (mode t) (inclus t)
                             &optional   (channel nil) (port nil))
                 (mki 'multi-seq 
                      :chord-seqs (loop for cs in (chord-seqs object)
                                        collect (ch-RM-sine cs sine %amp type mode inclus channel port))))


(om::defmethod! ring-sine ((accord chord) (sine t)
                              &optional (unit 'midic)
                              (type 'chord)
                              (output 'exclus))
   :initvals '(nil 6600  'midic 'chord 'exclus)
   :indoc '("accord" "sine" "unit" "type" "output")
   :menuins '((2 (("midic" 'midic) ("freq" 'freq) ))
             (3 (("chord" 'chord) ("seq" 'seq) ))
             (4 (("exclus" 'exclus) ("inclus" 'inclus))))
   :icon 137 
   :doc 
   "Simulates the ring modulation of each note of <accord> by one or several sine-waves. 
The optional argument <unit> determines whether sine is
entered in midicents, ('midic'), or in hertz ('freq').
When <sine> contains multiple pitches, the optional argument <type> is used 
to determine the format of the output  ('seq' or 'chord')
The optional argument <output> determines whether the original notes of 
<ch1> are included ('inclus') or excluded ('exclus') from the 
output chord or chords"
   (print unit)
   (let* ((l-freqs (mc->f (lmidic accord)))
          (l-vels (lvel accord))
          (l-durs (ldur accord))
          (l-offs (loffset accord))
          (modul (list! (if (equal unit 'midic) (mc->f sine) sine)))
          (nbcomp (1- (length l-freqs)))
          (nbmodul (1- (length modul)))
          (rm-freqs (if (and (equal output 'inclus) (equal type 'seq)) l-freqs nil))
          (rm-vels (if (and (equal output 'inclus) (equal type 'seq)) l-vels nil)) 
          (rm-durs (if (and (equal output 'inclus) (equal type 'seq)) l-durs nil)) 
          (rm-offs (if (and (equal output 'inclus) (equal type 'seq)) l-offs nil))
          lrm-freqs lrm-vels lrm-durs lrm-offs res)
     
     (for (m 0 1 nbmodul)
       (if (or (equal output 'exclus) (and (equal output 'inclus) (equal type 'seq) (> m 0)))
         (progn (setq rm-freqs nil)
                (setq rm-vels  nil )
                (setq rm-durs  nil) 
                (setq rm-offs  nil)))
       (if (and (equal output 'inclus) (equal type 'chord))
         (progn (setq rm-freqs  l-freqs)
                (setq rm-vels  l-vels )
                (setq rm-durs  l-durs) 
                (setq rm-offs  l-offs)))
       
       (for  (i 0 1 nbcomp)
         (push (list (+ (nth i l-freqs) (nth m modul)) (- (nth i l-freqs) (nth m modul))) rm-freqs)
         (push (create-list 2 (nth i l-durs)) rm-durs)
         (push (create-list 2 (nth i l-offs)) rm-offs)
         (push (create-list 2 (nth i l-vels)) rm-vels))
       (push (nreverse (flat rm-freqs)) lrm-freqs)
       (push (nreverse (flat rm-durs)) lrm-durs)
       (push (nreverse (flat rm-offs)) lrm-offs)
       (push (nreverse (flat rm-vels)) lrm-vels))
     
     (setq lrm-freqs (if  (or (one-elem sine) (equal type 'seq)) (flat (nreverse lrm-freqs))  
                          (nreverse lrm-freqs) ))
     (setq lrm-vels (if  (or (one-elem sine) (equal type 'seq)) (flat (nreverse lrm-vels))  
                         (nreverse lrm-vels) ))
     (setq lrm-durs (if  (or (one-elem sine) (equal type 'seq)) (flat (nreverse lrm-durs))  
                         (nreverse lrm-durs) ))
     (setq lrm-offs (if  (or (one-elem sine) (equal type 'seq)) (flat (nreverse lrm-offs))  
                         (nreverse lrm-offs) ))
     
     (if (or (one-elem sine) (equal type 'seq))
       
       (setf res  (make-instance 'chord 
                    :lmidic (f->mc (flat  lrm-freqs))
                    :ldur (flat  lrm-durs)
                    :loffset (flat  lrm-offs)
                    :lvel (flat  lrm-vels)))
       
       
       (for (m nbmodul -1 0)
         (push (make-instance 'chord 
                 :lmidic (f->mc (nth m lrm-freqs))
                 :ldur (nth m lrm-durs)
                 :loffset (nth m lrm-offs)
                 :lvel (nth m lrm-vels)) res)
         ))
     
     (ch-test-filter res '< 100 'lmidic)))


(om::defmethod! ring-sine ((accord chord-seq) (sine t)
                              &optional (unit 'midic)
                              (type 'chord)
                              (output 'exclus))

(mki 'chord-seq  
     :lmidic  (loop for  ch  in (chords accord)

                    do (print ch)
                    collect (ring-sine  ch sine unit 'chord output))
     :lonset (lonset accord)))








; .................. frequency shifting  .....................

; revoir - eventuellement prendre les fct esquisse

(defun simple-shift (fchord dfreq output)
  (let ((res (om+ dfreq fchord)))
    (if (equal output 'included) (x-append fchord res) res)))

(defun lcshift (fchord dfreq type output)
  (let ((res (deep-mapcar/1 #'om+ dfreq  fchord )))
    (if (eq output 'included) (setq res (mapcar #'x-append fchord res)) res)
    (if (eq type 'chord) (flat-once res) res)))

(defun lfshift (fchord dfreq type output)
  (let ((res (deep-mapcar/1 #'om+ dfreq  fchord )))
    (cond ((and (eq output 'included) (eq type 'chord))
           (mapcar #'(lambda (x) (x-append x fchord)) res))
          ((and (eq output 'included) (eq type 'chlist))
           (x-append fchord (flat-once res)))
          (t (if (eq type 'chord) (flat-once res) res)))))


(defun doubleshift (fchord dfreq type output)
  (cond ((eq type 'chord)
           (mapcar #'(lambda (x) (lfshift x dfreq type output)) fchord))
        ((and (eq output 'excluded) (eq type 'chlist))
           (flat-once (mapcar #'(lambda (x) (lfshift fchord x type output)) dfreq)))
        ((and (eq output 'included) (eq type 'chlist))
           (flat-once 
            (mapcar #'(lambda (x) (lcshift fchord x type output)) dfreq)))))



; semblable à fshift d'Esquisse (mais "output" manque dans Esquisse 

(defmethod! freq-shift ((chord t) (dfreq t) &optional 
                               (type 'chord) (output 'excluded) (unit 'midic))

  :initvals '('(4800 5250 5800) 100  'chord 'excluded 'midic)
  :indoc '("Chord" "D-freq" "Type" "Output" "Unit")
  :menuins '((2 (("chord" 'chord) ("chord list" 'chlist)))
            (3 (("excluded" 'excluded) ("included" 'included)))
            (4 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc  "Shifts the frequency of each note of <chord> by a frequency <dfreq> 
(positive or negative, but always in hertz).

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz. 

If <chord> is a list of chords the optional argument <type> is used to 
determine whether the output will be a list of chords ('seq'), each one
shifted by <dfreq> or a single chord combining the notes of all the 
shifted chords ('chord'). If <dfreq> is a list the same argument is used
to choose between a list of chords shifted by each successive <dfreq> or a 
single chord combining the different distortions. If both <chord> and
<dfreq> are lists the position 'seq' will return a list of chords 
containing  each chord shifted by each frequency; the position 'chord' 
will return a list of chords containing each chord shifted by all the 
listed frequencies.

The optional argument <output> determines whether the original <chord> is 
included  or excluded  from the output list."

(let ((listchord  (not (atom (car chord))))
      (listfreq (not (atom  dfreq)))
      (fchord (if (eq unit 'hz)  chord   (mc->f chord)))    res)
  (cond  ((and listchord listfreq)
             (setq res (doubleshift fchord dfreq type output)))
         (listchord (setq res (lcshift fchord dfreq type output)))
         (listfreq  (setq res (lfshift fchord dfreq type output)))
         (t (setq res (simple-shift fchord dfreq output))))
  (if (eq unit 'hz) res (f->mc res))))



;==========Rename freq-shift as fsh 22-11-2006==============================

(defmethod! fsh ((chord t) (dfreq t) &optional 
                               (type 'chord) (output 'excluded) (unit 'midic))

  :initvals '('(4800 5250 5800) 100  'chord 'excluded 'midic)
  :indoc '("Chord" "D-freq" "Type" "Output" "Unit")
  :menuins '((2 (("chord" 'chord) ("chord list" 'chlist)))
            (3 (("excluded" 'excluded) ("included" 'included)))
            (4 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc
  "Shifts the frequency of each note of <chord> by a frequency <dfreq> (positive or negative, but always in hertz).

The optional argument <unit> determines whether <chord> is entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the values will be converted to frequencies inside the function and then the output is reconverted to midicents. If 'freq' is selected the entry, calculations and output are all in hertz. 

If <chord> is a list of chords the optional argument <type> is used to determine whether the output will be a list of chords ('seq'), each one shifted by <dfreq> or a single chord combining the notes of all the shifted chords ('chord'). If <dfreq> is a list the same argument is used to choose between a list of chords shifted by each successive <dfreq> or a single chord combining the different distortions. If both <chord> and <dfreq> are lists the position 'seq' will return a list of chords containing  each chord shifted by each frequency; the position 'chord' will return a list of chords containing each chord shifted by all the listed frequencies.

The optional argument <output> determines whether the original <chord> is included  or excluded  from the output list."
   
  (freq-shift chord dfreq type output unit))

;=========================================================================




(defmethod! fshift-proc ((chord t) (dfreq number) (steps integer) 
                           &optional (output 'excluded) (unit 'midic))

  :initvals '('(4800 5250 5400) 400  3  'excluded 'midic)
  :indoc '("Chord" "D-freq" "Nb steps" "Output" "Unit")
  :menuins '((3 (("excluded" 'excluded) ("included" 'included)))
            (4 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc    "Progressively shifts <chord> until the final chord which is shifted by 
<dfreq> (positive or negative, but always in hertz). The argument <steps> 
determines the number of intermediate distortions to be produced between 
the unaltered <chord> and the chord shifted by <dfreq>. 

The argument <chord> may be a list, in which case the same process of 
shifting is carried out for each successive chord.

<dfreq> and <steps> may not be lists.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz.

The optional argument <output> determines whether the non-shifted <chord> 
is included  or excluded  from the output list of 
chords."

(let* ((ldfreq (n-arithm 0 dfreq steps)) 
       (ldfreq (if (equal output 'excluded) (cdr ldfreq) ldfreq))
       (res (freq-shift chord ldfreq output unit)))
(if  (not (atom (car chord))) (flat-once res) res)))



;==========Rename fshift-proc as fs-proc 22-11-2006=======================

(defmethod! fs-proc ((chord t) (dfreq number) (steps integer) 
                           &optional (output 'excluded) (unit 'midic))

  :initvals '('(4800 5250 5400) 400  3  'excluded 'midic)
  :indoc '("Chord" "D-freq" "Nb steps" "Output" "Unit")
  :menuins '((3 (("excluded" 'excluded) ("included" 'included)))
            (4 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc
  "Progressively shifts <chord> until the final chord which is shifted by <dfreq> (positive or negative, but always in hertz). The argument <steps> determines the number of intermediate distortions to be produced between the unaltered <chord> and the chord shifted by <dfreq>. 

The argument <chord> may be a list, in which case the same process of shifting is carried out for each successive chord.

<dfreq> and <steps> may not be lists.

The optional argument <unit> determines whether <chord> is entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the values will be converted to frequencies inside the function and then the output is reconverted to midicents. If 'freq' is selected the entry, calculations and output are all in hertz.

The optional argument <output> determines whether the non-shifted <chord> is included  or excluded  from the output list of chords."

  (fshift-proc chord dfreq steps output unit))

;=========================================================================





; .................  distorsions selon module "scaling"  ....................

(defun dist-midi (accord fund nhq1 nhq2 dint1 dint2)
  (let* ((minin (hqm fund nhq1))
         (maxin (hqm fund nhq2))
         (minout (+ minin dint1))
         (maxout (+ maxin dint2))) 
  (om-round (om-scale accord minout maxout minin maxin ))))

(defun dist-freq (accord fund nhq1 nhq2 dfq1 dfq2)
  (let* ((minin (hqf fund nhq1))
         (maxin (hqf fund nhq2))
         (minout (+ minin dfq1))
         (maxout (+ maxin dfq2))) 
  (f->mc/tm (om-scale (mc->f accord) minout maxout minin maxin ) )))


 (defun f->mc/tm (midics)
   (f->mc midics))



(defmethod!  d-harm/m ((fund number) (numer integer) (denom integer)
                           (begin integer) (end integer)  (nhq1 integer) (dint1 number)
                           (nhq2 integer)
                           (dint2 number)) 

  :initvals '(3600 1 1 1 7 1 0 7 0)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "N-harm1" "D-interv1" "N-harm2" "D-interv2")
  :icon 136 
  :doc  "rend une série harmonique distordue selon module 'scale'
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"

  (dist-midi (harm-ser fund  numer denom begin end ) fund nhq1 nhq2 dint1 dint2))




(defmethod!  d-harm/f ((fund number) (numer integer) (denom integer)
                              (begin integer) (end integer)  (nhq1 integer) 
                              (dfq1 number) (nhq2 integer)
                              (dfq2 number))

  :initvals '(3600 1 1 1 7 1 0 7 0)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "N-harm1" "D-freq1" "N-harm2" "D-freq2")
  :icon 136 
  :doc "rend une série harmonique distordue selon module 'scale'
On donne comme référence basse et haute 2 numéros d'harmoniques
et les distorsions correspondantes en hz (dfq1, dfq2)"

  (dist-freq (harm-ser fund  numer denom begin end ) fund nhq1 nhq2 dfq1 dfq2))



;====Rename d-harm/f and d-harm/m as dist-gen 22-11-2006=================================
;====<<input added for choice between midicents and frequency>>==========================

(defmethod! dist-gen ((fund number) (numer integer) (denom integer)
                          (begin integer) (end integer)  (nhq1 integer) 
                          (d1 number) (nhq2 integer)
                          (d2 number) (mc-or-f string))
  
  :initvals '(3600 1 1 1 7 1 0 7 0 "mc")
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "N-harm1" "Distance1" "N-harm2" "Distance2" "midicents or frequency")
  :icon 136 
  :doc "Returns a distorted (i.e. inharmonic) spectrum obtained through scaling of a harmonic series. Reference pitches are two partials <nhq1, nhq2> and the two midicent intervals <dint1, dint2> between the original harmonic pitches and the corresponding distorted pitch obtained after scaling. Input of the intervals <dint1, dint2> occurs in midicents or frequency.

N.B. Definition of the harmonic series (which partials are present of a given fundamental?) occurs by numerator and denominator. Distortion is the result of scaling the harmonic series."

  (if (string-equal mc-or-f "mc")
    (d-harm/m fund numer denom begin end nhq1 d1 nhq2 d2)
    (d-harm/f fund numer denom begin end nhq1 d1 nhq2 d2)))

;=======================================================================================



#|
;====Removed from menu 22-11-2006====================================================

;There seems to be a problem with double-float/2 division by zero??
; And with hqm, nth-overtones, and nth-harm (from esquisse lib) 

(defmethod! d-nth/m ((fund number) (nth list) (nhq1 integer) (dint1 number) (nhq2 integer) (dint2 number))

  :initvals '(3600 '(1 3 5 7 9) 1 0 9 0)
  :indoc '("Fundamental" "Ranks"  "N-harm1" "D-interv1" "N-harm2" "D-interv2")
  :icon 136 
  :doc  "rend une série harmonique distordue selon module 'scale'
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"

  (dist-midi (nth-overtones fund  nth) fund nhq1 nhq2 dint1 dint2))



(defmethod!  d-nth/f ((fund number) (nth list)
                      (nhq1 integer) (dfq1 number) (nhq2 integer) (dfq2 number))

  :initvals '(3600 '(1 3 5 7 9) 1 0 9 0)
  :indoc '("Fundamental" "Ranks"  "N-harm1" "D-freq1" "N-harm2" "D-freq2")
  :icon 136 
  :doc  "rend une série harmonique distordue selon module 'scale'
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"

  (dist-freq (nth-overtones fund  nth) fund nhq1 nhq2 dfq1 dfq2))
|#


;===================================================================================

(defmethod! distsym ((fund number ) (axe integer) (numer integer) (denom integer) 
                 (begin integer) (end integer)  
                 (nhq integer) (dfq number))

  :initvals '(3600 5 1 1 1 9 9 0)
  :indoc '("Fundamental" "Axis" "Numerator" "Denominator" "Begin" "End" "N-harm" "D-freq")
  :icon 136 
  :doc "série harmonique distordue selon module 'distor', autour d'un axe de symétrie
'axe' et une harm. de référence. Le crible numer/denom respecte l'axe de symétrie"

  (let* ((ecart (abs(- nhq axe)))
         (nhqhaut (+ axe ecart))
         (nhqbas (- axe ecart))
         (nhqbas (if (> nhqbas 0) nhqbas (- nhqbas 2)))   ; test hq inf
         (dfmul (/ (+ dfq (hqf fund nhq)) (hqf fund nhq)))
         (dfhaut (- (* (hqf fund nhqhaut) dfmul) (hqf fund nhqhaut)))
         (dfbas  (- (hqf fund nhqbas) (* (hqf fund nhqbas) dfmul) ))
         )
    (print (list nhqbas dfbas nhqhaut dfhaut ))
    (x-union (dist-freq (harm-ser fund numer denom axe begin) fund axe nhqbas 0 dfbas)
             (dist-freq (harm-ser fund numer denom axe end) fund axe nhqhaut 0 dfhaut)
         )))


;====Rename distsym as dist-sym 22-11-2006==============================================

(defmethod! dist-sym ((fund number ) (axe integer) (numer integer) (denom integer) 
                 (begin integer) (end integer)  
                 (nhq integer) (dfq number))

  :initvals '(3600 5 1 1 1 9 9 0)
  :indoc '("Fundamental" "Axis" "Numerator" "Denominator" "Begin" "End" "N-harm" "D-freq")
  :icon 136 
  :doc "Distortion of a harmonic series according to a symmetry axis <axe> and one reference partial <nhq>.

N.B. Definition of the harmonic series occurs by numerator and denominator. Distortion is the result of transforming the harmonic series around a symmetry axis. The employed numerator/denominator ratio is in accordance with the symmetry axis."

  (distsym fund axe numer denom begin end nhq dfq))

;=======================================================================================






;====All TO9 algorithms related functions removed from menu 22-11-2006===================
; .............  distorsions selon "algorithme TO9" (cf "Allégories")  ................

(defun fdist-to9 (accord fqmin qdist1 incrdist)
  (let ((ncomp (length accord)) fq res )
    (dotimes  (n ncomp)
         (setq fq (mc->f (nextl accord )))
         (setq fq (* fq (+ qdist1 (* incrdist (- fq fqmin)))))
         (push (f->mc fq) res))
       (reverse res)))
          
(defun h-dist (accord fund nhq1 nhq2 dint1 dint2)
  (let* ((minin (hqf fund nhq1))
         (maxin (hqf fund nhq2))
         (minout (* minin (cents->ratio dint1)))
         (maxout (* maxin (cents->ratio dint2)))
         (qdist1 (/ minout minin))
         (qdist2 (/ maxout maxin))
    (incrdist (/ (- qdist2 qdist1) (-  maxin minin))))
  (format t "minin = ~S maxin = ~S  minout = ~S maxout = ~S ~%" minin maxin minout maxout) 
  (fdist-to9 accord minin qdist1 incrdist)))


(defun fdist/hz (accord minref maxref  dmin  dmax )
"distord les fréquences de l'accord selon l'algorithme TO9.
On donne les notes de référence basse et haute (minref, maxref)
et les distorsions en hz correspondantes (dmin , dmax)"
(let* ((qdist1 (1+ (/ dmin (mc->f minref))))
      (qdist2 (1+ (/ dmax (mc->f maxref))))
      (incrdist (/ (- qdist2 qdist1) (- (mc->f maxref) (mc->f minref)))))
  (fdist-to9 accord (mc->f minref) qdist1 incrdist)))



(om::defmethod! dist-overtones ((fund t) (numer integer) (denom integer)
                                   (begin integer) (end integer)  (nhq1 integer) (nhq2 integer)
                                   (dint1 number) (dint2 number))
  :initvals '(2400 1 1 1 7 1 0 7 0)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "nhq1" "nhq2"
           "dint1" "dint2")
  :icon 136 
  :doc 
"rend une série harmonique distordue selon l'algorithme TO9.
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"

  (let* ((accord (harm-series fund numer denom begin end 1 2))
         (minin (hqf fund nhq1))
         (maxin (hqf fund nhq2))
         (minout (* minin (cents->coef dint1)))
         (maxout (* maxin (cents->coef dint2)))
         (qdist1 (/ minout minin))
         (qdist2 (/ maxout maxin))
         (incrdist (/ (- qdist2 qdist1) (-  maxin minin))))
  (format t "minin = ~S maxin = ~S  minout = ~S maxout = ~S ~%" minin maxin minout maxout) 
  (fdist-to9 accord minin qdist1 incrdist)))



(om::defmethod! dist-to9 ((fund number) (numer integer) (denom integer)
                             (begin integer) (end integer)  (nhq1 integer) (dint1 number) 
                             (nhq2 integer)
                             (dint2 number))
  :initvals '(3600 1 1 1 7 1 0 7 0)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "N-harm1" "D-freq1"
           "N-harm2" "D-freq2")
  :icon 136 
  :doc "rend une série harmonique distordue selon l'algorithme TO9.
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"
  (let* ((accord (polysp fund 0 numer denom begin end 0 nil))
         (minin (hqf fund nhq1))
         (maxin (hqf fund nhq2))
         (minout (* minin (cents->ratio dint1)))
         (maxout (* maxin (cents->ratio dint2)))
         (qdist1 (/ minout minin))
         (qdist2 (/ maxout maxin))
         (incrdist (/ (- qdist2 qdist1) (-  maxin minin))))
  (format t "minin = ~S maxin = ~S  minout = ~S maxout = ~S ~%" minin maxin minout maxout) 
  (fdist-to9 accord minin qdist1 incrdist)))



(om::defmethod!  disto9-nth/m ((fund number) (nth list)
                      (nhq1 integer) (dint1 number) (nhq2 integer) (dint2 number))

  :initvals '(3600 '(1 3 5 7 9) 1 0 9 0)
  :indoc '("Fundamental" "Ranks" "N-harm1" "D-interv1" "N-harm2" "D-interv2")
  :icon 136 
  :doc "rend une série harmonique distordue selon l'algorithme TO9.
On entre une liste d'harmoniques
On donne comme référence basse et haute 2 numéros d'harmoniques
et les intervalles midic de distorsion correspondants (dint1, dint2)"
  
  (h-dist (n-harm fund nth) fund nhq1 nhq2 dint1 dint2))



(om::defmethod! disto9-nth/f  ((fund number) (nth list)
                      (nhq1 integer) (dfq1 number) (nhq2 integer) (dfq2 number))
  :initvals '(3600 '(1 3 5 7 9) 1 0 9 0)
  :indoc '("Fundamental" "Ranks" "N-harm1" "D-freq1" "N-harm2" "D-freq2")
  :icon 136 
  :doc  "rend une série harmonique distordue selon l'algorithme TO9.
On entre une liste d'harmoniques
On donne comme référence basse et haute 2 numéros d'harmoniques
et les distorsions correspondantes en hz (dfq1, dfq2)"
  
  (fdist/hz (n-harm fund nth) (hqm fund nhq1) (hqm fund nhq2 )
                      dfq1 dfq2) )

;===============================================================================












; ............... distorsion des fréquences ....................


; semblable à fdistor d'Esquisse (mais "output" manque dans Esquisse) 

(defmethod! freq-distor  ((chord t) (minout number) (maxout number) 
                                 &optional (minin nil) (maxin nil)  
                                 (output 'excluded ) (unit 'midic ))
  
  :initvals '('(4800 5250 5400) 5700 6300  () ()  'excluded 'midic)
  :indoc '("Chord" "minout" "maxout" "minin" "maxin" "output" "unit")
  :menuins '((5 (("excluded" 'excluded) ("included" 'included)))
            (6 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc   "Distorts the frequencies of <chord> so that the lowest note is changed to
<minout> and the highest note to <maxout>. Interior notes are rescaled so
as to preserve the relative positions of their frequencies.

The optional inputs <minin> and <maxin> allow the scaling to be done 
relative to two selected reference notes rather than the highest and 
lowest notes of the chord. The note entered as <minin> will be moved to 
<minout>, and <maxin> to <maxout> the rest of the chord is then 
rescaled accordingly.

If <chord> is a list of chords, output will be a corresponding list of 
distorted chords.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted 
<chord> is included ('inclu') or excluded ('exclu') from the output list. 
If included the non-distorted notes will be mixed with the distorted into
a single chord."
  
  (let ((minin (if (null minin) (list-min chord) (car! minin)))
        (maxin (if (null maxin) (list-max chord) (car! maxin))))

    (if (equal unit 'midic) (setq chord (mc->f chord) minout (mc->f minout) maxout (mc->f maxout)
                               minin  (mc->f minin)
                               maxin  (mc->f maxin)))

    (let ((res (om-scale chord (car! minout) (car! maxout) minin maxin)))
      (setq res (if (equal output 'included) (if (atom (car chord)) (x-append chord res) 
                                                 (mapcar #'x-append chord res)) res))
      (if (equal unit 'hz) res (f->mc res) ))))

;===============Rename freq-distor as disto 22-11-2006===================================

(defmethod! disto ((chord t) (minout number) (maxout number) 
                                 &optional (minin nil) (maxin nil)  
                                 (output 'excluded ) (unit 'midic ))
  
  :initvals '('(4800 5250 5400) 5700 6300  () ()  'excluded 'midic)
  :indoc '("Chord" "minout" "maxout" "minin" "maxin" "output" "unit")
  :menuins '((5 (("excluded" 'excluded) ("included" 'included)))
            (6 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc   "Distorts the frequencies of <chord> so that the lowest note is changed to
<minout> and the highest note to <maxout>. Interior notes are rescaled so
as to preserve the relative positions of their frequencies.

The optional inputs <minin> and <maxin> allow the scaling to be done 
relative to two selected reference notes rather than the highest and 
lowest notes of the chord. The note entered as <minin> will be moved to 
<minout>, and <maxin> to <maxout> the rest of the chord is then 
rescaled accordingly.

If <chord> is a list of chords, output will be a corresponding list of 
distorted chords.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted 
<chord> is included ('inclu') or excluded ('exclu') from the output list. 
If included the non-distorted notes will be mixed with the distorted into
a single chord."

  (freq-distor chord minout maxout minin maxin output unit))

;==========================================================================================



(defmethod! fdistor-proc ((chord t) (steps integer) (minout number) (maxout number) 
                                 &optional (minin nil) 
                                 (maxin nil) (output 'excluded ) (unit 'midic ))

  :initvals '('(4800 5250 5400) 3 5700 8500  () ()  'excluded 'midic)
  :indoc '("Chord" "steps" "minout" "maxout" "minin" "maxin" "output" "unit")
  :menuins '((6 (("excluded" 'excluded) ("included" 'included)))
            (7 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc    "Progressively distorts <chord> until the distortion specified by <minout> 
and <maxout> is reached. The argument <steps> determines the number of 
intermediate distortions to be produced between the unaltered <chord> and 
the final distortion. (For explanation of frequency distortion, as well as 
the use of <minout>,<maxout>,<minin> and <maxin> see the box 'fdistor') 

<chord> may not be a list of chords.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted 
<chord> is included ('inclu') or excluded ('exclu') from the output list 
of chords."
(let*  ((valmin (if (null minin) (list-min (list! chord)) (car! minin)))
        (valmax (if (null maxin) (list-max (list! chord)) (car! maxin)))
        (stepmin (if (/= steps 0) (/ (- (car! minout) valmin) steps) 0))
        (stepmax (if (/= steps 0) (/ (- (car! maxout) valmax) steps) 0))
        (deb (if (equal output 'included) 0 1 ))
         res)
    (for (n deb 1 steps) 
      (newl res (freq-distor chord (round (+ valmin (* n stepmin))) (round (+ valmax (* n stepmax)))
                         (car! minin) (car! maxin) 1 unit)))
    (nreverse res)))

;================Rename fdistor-proc as dist-proc 22-11-2006====================================

(defmethod! dist-proc ((chord t) (steps integer) (minout number) (maxout number) 
                                 &optional (minin nil) 
                                 (maxin nil) (output 'excluded ) (unit 'midic ))

  :initvals '('(4800 5250 5400) 3 5700 8500  () ()  'excluded 'midic)
  :indoc '("Chord" "steps" "minout" "maxout" "minin" "maxin" "output" "unit")
  :menuins '((6 (("excluded" 'excluded) ("included" 'included)))
            (7 (("midic" 'midic) ("hz" 'hz) )))
  :icon 137
  :doc
  "Progressively distorts <chord> until the distortion specified by <minout> and <maxout> is reached. The argument <steps> determines the number of intermediate distortions to be produced between the unaltered <chord> and 
the final distortion (For explanation of frequency distortion, as well as the use of <minout>, <maxout>,  <minin> and <maxin> see the box 'fdistor').

<chord> may not be a list of chords.

The optional argument <unit> determines whether <chord> is entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the values will be converted to frequencies inside the function and then the output is reconverted to midicents. If 'freq' is selected the entry, calculations and output are all in hertz.

The optional argument <output> determines whether the non-distorted <chord> is included ('inclu') or excluded ('exclu') from the output list of chords."
  (fdistor-proc chord steps minout maxout minin maxin output unit))

;=======================================================================================









;====All TO9 algorithms related functions removed from menu 22-11-2006===================

;  distorsions selon algo TO9

(om::defmethod! fdisto9/hz ((accord list) (minref number) (maxref number) 
                      (dmin number) (dmax number))

  :initvals '('(4800 5250 5400) 4800 5400  100 250)
  :indoc '("accord"  "minref" "maxref" "dmin" "dmax")
  :icon 136
  :doc   "distord les fréquences de l'accord selon l'algorithme TO9.
On donne les notes de référence basse et haute (minref, maxref)
et les distorsions en hz correspondantes (dmin , dmax)"
  (fdist/hz accord minref maxref  dmin  dmax ))
    


(om::defmethod! fdisto9/note ((accord list) (minin number) (maxin number) 
                      (minout number) (maxout number))
  :initvals '('(4800 5250 5400) 4800 5400 3200 7000)
  :indoc '("accord"  "minin" "maxin" "minout" "maxout")
  :icon 136
  :doc  "distord les fréquences de l'accord selon l'algorithme TO9.
On donne les notes de référence basse et haute (minin, maxin)
et les notes distordues correspondantes (minout , maxout)"
(let* ((qdist1 (/ (mc->f minout) (mc->f minin)))
      (qdist2 (/ (mc->f maxout) (mc->f maxin)))
      (incrdist (/ (- qdist2 qdist1) (- (mc->f maxin) (mc->f minin)))))
  (fdist-to9 accord (mc->f minin) qdist1 incrdist)))


(om::defmethod! fdisto9/interv ((accord list) (ncomp1 integer) (ncomp2 integer) 
                      (dint1 number) (dint2 number))
  :initvals '('(4800 5250 5400) 0 2  100 300)
  :indoc '("accord"  "ncomp1" "ncomp2" "dint1" "dint2")
    :icon 136
  :doc  "distord les fréquences de l'accord selon l'algorithme TO9.
On donne comme référence basse et haute 2 numéros de composantes (à partir de 0)
et les intervalles midic de distorsion correspondants (dint1, dint2)"
  (let* ((minin (nth ncomp1 accord))
         (maxin (if (or (= ncomp2 0) (> ncomp2 (1- (length accord)))) (car (last accord))
                         (nth ncomp2 accord)))
         (minout (+ dint1 minin))
         (maxout (+ dint2 maxin))
         (qdist1 (/ (mc->f minout) (mc->f minin)))
         (qdist2 (/ (mc->f maxout) (mc->f maxin)))
         (incrdist (/ (- qdist2 qdist1) (- (mc->f maxin) (mc->f minin)))))
  (fdist-to9 accord (mc->f minin) qdist1 incrdist)))


;===============================================================================



;---------------------------Other Treatments------------------------------------

(om::defmethod! fq-interpol ((begin t) (end t) (steps integer) (curve number))
  :initvals '(4800 6000 5 1.0)
  :indoc '("begin"  "end" "steps" "curve")
    :icon 136
  :doc "interpolation calculée sur les fréquences. Entrée et sortie en midics"
  (f->mc (interpolation (mc->f begin) (mc->f end) steps (float curve))))



;====Rename fq-interpol as f-interpol 23-06-2007================================

(om::defmethod! f-interpol ((begin t) (end t) (steps integer) (curve number))
  :initvals '(4800 6000 5 1.0)
  :indoc '("begin"  "end" "steps" "curve")
    :icon 136
  :doc "Interpolation calculated between frequencies. Input and output are in midicents."
  
  (fq-interpol begin end steps curve))

;===============================================================================





(om::defmethod! densif/f ((accord list) (density integer) (mmin number) (mmax number))
  :initvals '((4800 5300 5900) 1 4800 5900)
  :indoc '("accord"  "density" "mmin" "mmax")
    :icon 136
  :doc  "ajoute <density> partiels entre chaque composante du spectre comprise
entre mmin et mmax (midics). Les partiels créés divisent les intervalles 
de l'accord en <density> intervalles égaux en fréquence"
  (f->mc (densifier  (mc->f accord) density  0 (mc->f mmin) (mc->f mmax))))


;====Rename densif/f as f-densifier 23-06-2007===================================
;====full function in definition in order to avoid "/"in name====================

(om::defmethod! f-densifier ((accord list) (density integer) (mmin number) (mmax number))
  :initvals '((4800 5300 5900) 1 4800 5900)
  :indoc '("accord"  "density" "mmin" "mmax")
    :icon 136
  :doc  "Adds <density> partials between every component pair of the spectrum within a given range defined by <mmin> and <mmax>. The created partials divide each frequency interval of the original spectrum into <density> intervals of equal lengths."

  (f->mc (densifier  (mc->f accord) density 0 (mc->f mmin) (mc->f mmax))))


;================================================================================


(defun reharm-fct (accord toler)
  (let ((vf (virt-fund accord toler 'midic)) 
        (rgh (closest-harm   accord (virt-fund accord toler 'midic) 'ranks 'midic)))
  (print (list  vf (mc->n (approx-m vf 8))))
  (print rgh)
  (n-harm vf rgh)))

(om::defmethod! reharmoniser ((accords list) (toler number ))
  :initvals '('(4800 5340 5987 6250) 25 )
  :indoc '("accords"  "toler" )
    :icon 136
  :doc  "rend les accords plus harmoniques ; effet plus ou moins grand selon <toler>"
  (less-deep-mapcar #'reharm-fct  accords toler))


;====Rename reharmoniser as reharmonizer 23-06-2007==============================
;====added object compatibility==================================================

(om::defmethod! reharmonizer ((accords list) (toler number))
  :initvals '('(4800 5340 5987 6250) 25 )
  :indoc '("accords"  "toler" )
    :icon 136
  :doc
"Returns a more harmonic chord, a chord more related to a harmonic series. 
The amount of increase in harmonicity is determined by <toler>. 
The reharmonizer function first determines the virtual fundamental of the chord and replaces the original pitches by the closest harmonic partials on the obtained virtual fundamental. Accepts list of midicents and list of lists of midicents, and objects (chords, chord-seq and multi-seq)."
  
(reharmoniser accords toler))


(om::defmethod! reharmonizer ((self chord) (toler number))
  (make-instance 'chord
    :lmidic
    (reharmonizer  (lmidic self) toler)
    :ldur (ldur self)
    :lvel (lvel self)
    :loffset (loffset self)
    :lchan (lchan self)))


(om::defmethod! reharmonizer ((self chord-seq) (toler number))
  (make-instance 'chord-seq
    :lmidic
    (reharmonizer (mapcar 'lmidic (chords self)) toler)
    :lonset (lonset self)
    :ldur (ldur self)
    :lvel (lvel self)
    :loffset (loffset self)
    :lchan (lchan self)
    :legato (legato self)))



(om::defmethod! reharmonizer ((self multi-seq) (toler number))
  (make-instance 'multi-seq 
    :chord-seqs 
    (loop for chord-seq in  (chord-seqs self)
          collect 
          (reharmonizer chord-seq toler))))



;=============================================================================



(om::defmethod! mul-freq ((ch1 list ) (ch2 list ) (type symbol ))
                                     
  :initvals '('(4800 5300 6000) '(6200 6700 7000)  'chlist)
  :indoc '("ch1"  "ch2" "type" )
  :menuins '((2 (("chlist" 'chlist) ("chord" 'chord))))
    :icon 136
  :doc   "Like mul-chord, but calculations are made with frequencies.
The optional argument <type> allows the choice of whether the output is
a list of chords ('seq') or a single chord ('chord') containing all the 
transpositions combined."
   (let ((ch1 (list! (mc->f ch1))) 
         (addfq  (dx->x 0 (inter->freq ch2 ))) 
          res)
      (dolist (n ch1 )
        (push (om+ n addfq) res))
        (setq res (nreverse (f->mc res)))
        (if (equal type 'chord) (flat res) res )))


;====Rename mul-freq as f-multiplier 24-06-2007==============================

(om::defmethod! f-multiplier ((ch1 list ) (ch2 list ) (type symbol ))
                                     
  :initvals '('(4800 5300 6000) '(6200 6700 7000)  'chlist)
  :indoc '("ch1"  "ch2" "type" )
  :menuins '((2 (("chlist" 'chlist) ("chord" 'chord))))
  :icon 136
  :doc   "Like mul-chord, but calculations are made with frequencies. The optional argument <type> allows the choice of whether the output is a list of chords ('seq') or a single chord ('chord') containing all the transpositions combined."
  
  (mul-freq ch1 ch2 type))

;=============================================================================




; ----------------------  analyse ----------------------

(defun best-freq1 (freqs)
  (let ((sum 0) (nb-freq (length freqs)))
    (while freqs (incf sum (log (nextl freqs))))
    (exp (/ sum nb-freq))))


(defmethod! best-freq ((chord list) (unit symbol))
  :initvals '('(6000)  'midic)
  :indoc '("pitches" "unit" )
  :menuins '((1 (("Midics" 'midic) ("Freqs" 'freq)))  )
  :icon 136 
  :doc     
  
  "Returns the note which is at the minimum possible distance from the 
frequencies of all the notes of <chord>. (minimum sum of the squares of 
the distances) This note can be thought of as a sort of center of gravity
for <chord> (it is not usually a member of the chord).

If <chord> is a list of chords the box returns a list of best frequencies.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz."
  (if (eq unit 'freq)
    (less-deep-mapcar 'best-freq1 chord)
    (f->mc (less-deep-mapcar 'best-freq1 (mc->f chord)))))


(defmethod! best-freq ((chord chord) (unit symbol))
   (best-freq (lmidic chord) 'lmidic))

(defmethod! best-freq ((chord chord-seq) (unit symbol))
   (best-freq (lmidic chord) 'lmidic))


;====Rename best-freq as center-freq 25-06-2007=========

(defmethod! center-freq ((chord list) (unit symbol))
  :initvals '('(6000)  'midic)
  :indoc '("pitches" "unit" )
  :menuins '((1 (("Midics" 'midic) ("Freqs" 'freq)))  )
  :icon 136 
  :doc     
  
  "Returns the note which is at the minimum possible distance from the 
frequencies of all the notes of <chord> (minimum sum of the squares of 
the distances). This note can be thought of as a sort of center of gravity
for <chord> (it is usually not a member of the chord).

If <chord> is a list of chords the box returns a list of center frequencies.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz."

  (best-freq chord unit))


(defmethod! center-freq ((chord chord) (unit symbol))
   
  (best-freq chord unit))


(defmethod! center-freq ((chord chord-seq) (unit symbol))
   (best-freq chord unit))


;========================================================



(defun closest-harm-f (f0 freq )
  ""
  (if (<= freq f0) (/ f0 freq)
      (let* ((ratio (/ freq f0))
             (n-partial (floor ratio))
             (d1 (/ ratio n-partial))
             (d2 (/ (1+ n-partial) ratio)))
        (if (< d1 d2) n-partial (1+ n-partial) ))))


(om::defmethod! closest-harm ((chord list) (fund number) (type symbol)
                   &optional (unit 'midic))
                   

  :initvals '('(4800 5250 5580) 2400 'notes 'midic)
  :indoc '("chord"  "fund" "type" "unit")
  :menuins '((2 ( ("notes" 'notes) ("ranks" 'ranks)))
            (3  (("midic" 'midic) ("hz" 'hz))))
  :icon 137
  :doc
  "Calculates the closest partial of the harmonic series built on <fund> to each note of <chord> (For explanations on building harmonic series, see the object 'n-sp-gen').

If <chord> is a list of chords the result will contain the analyses of each successive chord.

The optional argument <unit> determines whether <chord> is entered in midicents, or in hertz. If 'midic' is selected the values will be converted to frequencies inside the function and then the output (if appropriate) is reconverted to midicents. If 'hz' is selected the entry, calculations and output (if appropriate) are all in hertz.

The optional argument <type> determines whether the output is a list of partial numbers/ranks or the actual notes corresponding to those partials." 

(let* ((ffund  (car! (if (equal unit 'midic) (mc->f fund) fund)))
       (fchord (if (equal unit 'midic) (mc->f chord) chord))
       (res (deep-mapcar/1 #'(lambda (x) (closest-harm-f ffund x)) fchord)))
  (if (equal type 'notes) (n-harm  fund res ) res)))
            

(om::defmethod! deviations  ((accord list) (fond number) (numer integer)  (denom integer) 
                     (begin integer) (end integer) (m/f symbol))

  :initvals '('(4800 5250 5580) 3200 1 1 1 1 'm)
  :indoc '("chord"  "fundamental" "numerator" "denominator" "begin" "end" "m/f")
  :icon 136
  :doc "Deviations of the pitches/frequencies of a distorted chord when compared to its harmonic counterpart; m for midicents and f for frequencies."

  (if (equal m/f 'm) (om- accord (nth-harm fond numer denom begin end)) 
      (om- (mc->f accord) (mc->f (nth-harm fond numer denom begin end)))))


;====Removed from menu 22-11-2006========================

(om::defmethod! l-interfreq ((midics t))
  :initvals '('(4800 5250 5580))
  :indoc '("midics" )
    :icon 136
  :doc "liste des intervalles en freq entre les notes successives
d'une liste de midics"

  (carlist! (x->dx (mc->f midics))))

;=========================================================

(om::defmethod! quelle-harm ((accord list) (fond number) (approx integer)) 
  :initvals '('(4800 5250 5580) 2400 4)
  :indoc '("accord"  "fond" "approx")
    :icon 136
  :doc  "donne le rang harm par rapport à une fond. et une approx"
(om-round (om/ (mc->f (approx-m accord approx)) (car! (mc->f fond))) 2))


;===Rename quelle-harm as which-harm 25-06-2007============

(om::defmethod! which-harm ((chord list) (fund number) (approx integer)) 
  :initvals '('(4800 5250 5580) 2400 4)
  :indoc '("chord"  "fund" "approx")
    :icon 136
  :doc  "Returns the partial number for a given fundamental and approximation."

  (quelle-harm chord fund approx))

;==========================================================

(om::defmethod!  interfreq ((midics list) (nbdec integer))
  :initvals '('(4800 5250 5580) 2)
  :indoc '("midics"  "nbdec")
    :icon 136
  :doc "liste des intervalles en freq entre les notes successives
d'une liste de midics"
  (om-round (carlist! (x->dx (mc->f midics))) nbdec))


(defun calc-distor (rang accord)
  (/ (log (/ (mc->f (nth (1- rang) accord )) (mc->f (list-min  accord)))) (log rang)))

(defun calc-distor1 (rang accord )
  (print rang)
  (/ (log (/ (mc->f (list-max accord )) (mc->f (list-min  accord)))) (log rang)))

(defun quelle-dist-1 (accord rang mode)
  (om-round (- (* 100
     (cond ((equal mode 'all)
            (om-mean (car-mapcar 'calc-distor (arithm-ser 2 (length accord) 1) accord) 1))
           ((equal mode 'spectrum)   (calc-distor rang accord ))
           ((equal mode 'one-harm) (calc-distor1 rang accord ))))
     100) 4))

;===Rename interfreq as inter-freq 25-06-2007============

(om::defmethod!  inter-freq ((midics list) (nbdec integer))
  :initvals '('(4800 5250 5580) 2)
  :indoc '("midics"  "nbdec")
  :icon 136
  :doc "Returns a list of frequency intervals between successive notes defined by a midicents list and rounds off the values to a specified amount of decimals <nbdec>."

  (interfreq midics nbdec))

;==========================================================

(om::defmethod! quelle-dist ((accords list) (rang integer) (mode symbol))
  :initvals '('(2400 3636 4360 4872 5270 5596 5870)  2 'one-harm)
  :indoc '("accords"  "rang" "mode")
  :icon 136
  :menuins '((2 (("une-harm" 'one-harm) ("spectre" 'spectrum) ("toutes" 'all))))
  :doc "rend la valeur de la distorsion harmonique (en %) d'un agrégat.
La note la plus grave est censée Ítre la fondamentale;
on effectue le calcul par rapport à un partiel dont on donne le rang.
On choisissant l'option <toutes> on obtient la moyenne des distorsions calculées à partir de chaque 
partiel - accepte une liste d'accords"
  (less-deep-mapcar 'quelle-dist-1 accords rang mode))


;===Rename quelle-dist as which-dist 25-06-2007============

(om::defmethod! which-dist ((chords list) (rank integer) (mode symbol))
  :initvals '('(2400 3636 4360 4872 5270 5596 5870)  2 'one-harm)
  :indoc '("chords"  "rang" "mode")
  :icon 136
  :menuins '((2 (("one-harm" 'one-harm) ("spectrum" 'spectrum) ("all" 'all))))
  :doc "Returns the distortion percentage of a chord (or a list of chords). The lowest note of the chord is assumed to be the fundamental; the calculation is based on one partial and its assigned number/rank. When choosing the option <all>, the mean of the distortions for all partials is obtained."

  (quelle-dist chords rank mode))

;==========================================================




; -------  fondamentales virtuelles  --------------

(defun cents->coef (nb-cents)
  "<cents->coef> takes an interval expressed in midi-cents and returns the ratio 
between two frequencies separated by that interval; i.e., the value: (freq + <nb-
cents>) / freq."
  (expt 2.0 (/ nb-cents 1200.0)))

(defun virt-fund1 (chord cents)
  (car-mapcar #'(lambda (c) (fond-virt-f  chord (1- (cents->coef c)) ;;(round (/ 100 c))
                                          )) cents))

(defun fond-virt-f (freqs approx)
  (tolerant-gcd freqs approx))

;;From Gerard Assayag [93 07 16]

(defun tolerant-gcd (values grid-ratio)
  "floating gcd with tolerance grid-ratio around the values."
  (labels ((grid-above (val) (* val (1+ grid-ratio)))
           (grid-below (val) (/ val (1+ grid-ratio)))
           (gcd-try (values gcd-min gcd-max)
             (when (<= gcd-min gcd-max)
               (ifnot values
                 (/ (+ gcd-min gcd-max) 2.0)
                 (let* ((val-below (grid-below (first values)))
                        (val-above (grid-above (first values)))
                        (quo-min (ceiling (/ val-below gcd-max)))
                        (quo-max (floor (/ val-above gcd-min))))
                   (do* ((quotient quo-min (1+ quotient)) (gcd-interval))
                        ((> quotient quo-max) nil)
                     (setf gcd-interval
                           (gcd-try (rest values)
                                         (max gcd-min (/ val-below quotient))
                                         (min gcd-max (/ val-above quotient))))
                     (when gcd-interval
                       (return-from gcd-try gcd-interval))))))))
    (gcd-try values .1 (grid-above (apply 'min values)))))

(defmethod! virt-fund ((chord list) (cents integer) (unit symbol)) 

   :initvals '('(6000) 50  'midic)
   :indoc '("pitches""approx" "unit" )
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq)))  )
   :icon 136
   :doc     

  "Returns the highest fundamental for which the notes of <chord> could be 
thought of as harmonic partials. In general, the lower the virtual 
fundamental, the more dissonant the chord. 

<chord> may  be a list of midics, a list of list of midics, a chord object or a chord-seq objest.

The argument <cents> determines the precision of the analysis (a value of 
'0' would return the real fundamental; the larger the value the more 
approximate the result).

If <chord> is a sequence of chords the box returns a list of virtual 
fundamentals.

The optional argument <unit> determines whether <chord> is entered and the 
result returned in midicents, ('midic'), or in hertz ('freq'). The argument
<cents>, however, remains unchanged."

  (if (eq unit 'freq)
    (less-deep-mapcar 'virt-fund1 chord cents)
    (f->mc (less-deep-mapcar 'virt-fund1 (mc->f chord) cents))))

(defmethod! virt-fund ((chord chord) (cents integer) (unit symbol)) 
   (virt-fund (lmidic chord) cents 'midic))

(defmethod! virt-fund ((chord chord-seq) (cents integer) (unit symbol)) 
   (virt-fund (lmidic chord) cents 'midic))


;===Rename virt-fund as virtual-fund in order to avoid clashes with Esquisse 26-06-2007===

(defmethod! virtual-fund ((chord list) (cents integer) (unit symbol)) 

   :initvals '('(6000) 50  'midic)
   :indoc '("pitches""approx" "unit" )
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq)))  )
   :icon 136 
   :doc
   "Returns the highest fundamental for which the notes of <chord> could be  thought of as harmonic partials. In general, the lower the virtual fundamental, the more dissonant the chord. 

<chord> may  be a list of midics, a list of list of midics, a chord object, a list of chord objects or a chord-seq object. If <chord> is a sequence of chords the box returns a list of virtual fundamentals.

The argument <cents> determines the precision of the analysis (A value of '0' would return the real fundamental; the larger the value the more approximate the result). The optional argument <unit> determines whether <chord> is entered and the result returned in midicents ('midic'), or in hertz ('freq')."

  (virt-fund chord cents unit))

(defmethod! virtual-fund ((chord chord) (cents integer) (unit symbol)) 
   
   (virt-fund chord cents unit))

(defmethod! virtual-fund ((chord chord-seq) (cents integer) (unit symbol)) 
   
   (virt-fund chord cents unit))

;==================================================================

(om::defmethod! fonds-virts ((accord list )  (pmin number ) (pmax number ) 
                     (pstep number ) (approx integer )) 
  :initvals '('(2400 3636 4360 4872 5270 5596 5870)  5 100 5 4)
  :indoc '("accord"  "pmin" "pmax" "pstep" "approx")
  :icon 136
  :doc  "liste des fond virt obtenues en faisant varier la précision de pmin à pmax
avec un pas pstep. Les valeurs redondantes à <approx> prËs sont éliminées
de la liste"
  (let (res)
    (for (i pmin pstep pmax) 
      (push (virt-fund accord i 'midic) res))
    (unique-notes (nreverse res) approx)))

(om::defmethod! fonds-virts ((accord chord )  (pmin number ) (pmax number ) 
                     (pstep number ) (approx integer )) 
(fonds-virts (lmidic accord) pmin pmax pstep approx))


;====Rename fonds-virts as virt-fund-step 25-06-2007================

(om::defmethod! virt-fund-step ((accord list)  (pmin number) (pmax number) 
                     (pstep number) (approx integer)) 
  :initvals '('(2400 3636 4360 4872 5270 5596 5870)  5 100 5 4)
  :indoc '("accord"  "pmin" "pmax" "pstep" "approx")
  :icon 136
  :doc  "Returns a list of virtual fundamentals obtained after varying the precision of the calculation from <pmin> to <pmax> by steps with size <pstep>. After approximation redundant pitches are removed."

  (fonds-virts accord pmin pmax pstep approx))

(om::defmethod! virt-fund-step ((accord chord)  (pmin number ) (pmax number ) 
                     (pstep number ) (approx integer ))

  (fonds-virts accord pmin pmax pstep approx))

;======================================================================



;;; Fondamentales virtuelles multiples. Algorithme par Olivier Delerue.

(defmethod! multi-virt-fun ((chord list) (approx integer)  (thresh number)  (unit symbol))
   :initvals '('(6000 6400 6700) 50 1200 'midic)
   :indoc '("Chord" "Approx" "MinFund" "Unit")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq))))
   :icon 242 
   :doc  
   "Computes a series of possible virtual fundamentals from <chord> (a list of pitches) and <approx> (in midicents).
The result is a list of chord-sequences that can be fed to the 'chord-seq'input of a 'multi-seq'. Each chord-seq is a possible solution, sorted from the least significant to the most significant. Each chord-seq is a series of chords where the lowest note is a virtual fundamental and the remaining notes are a subset
of the original chord.
<thresh> is a minimum pitch/frequency value for the virtual fundamentals. If the menu <unit> is 'freq' then <chord> and <thresh> must be given in Hz, otherwise in Midics."
   
   (setf chord (sort (copy-list chord) '<))
   (when (eq unit 'midic) (setf chord (mc->f chord) thresh (mc->f thresh)))
   (let ((classement (make-classement chord  (cents->coef approx) thresh)) )
     (loop while  (iteration classement ) )
     (loop for regroupement in (rest (regroupements classement))
           collect
           (make-instance 'chord-seq :lmidic 
                          (loop for spectre in (spectres regroupement)
                                collect   
                                (f->mc (join-fund-to-spec (first (fondamentales spectre))
                                                          (mapcar 'frequence (partiels spectre)))))))))
     


(defmethod! multi-virt-fun ((chord chord) (approx integer)  (thresh number)  (unit symbol))
  (m-vir-fun (lmidic chord) approx thresh 'midic))

(defun make-classement (liste-partiels tolerance freq-min)
  (make-instance 'classement     
    :regroupements (list (make-instance 'regroupement 
                           :spectres (cree-liste-spectres liste-partiels  tolerance freq-min)))))


;====Rename multi-virt-fun as virt-fund-multi 25-06-2007========================

(defmethod! virt-fund-multi ((chord list) (approx integer)  (thresh number)  (unit symbol))
   :initvals '('(6000 6400 6700) 50 1200 'midic)
   :indoc '("Chord" "Approx" "MinFund" "Unit")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq))))
   :icon 136 
   :doc  
   "Computes a series of possible virtual fundamentals from <chord> (a list of pitches) and <approx> (in midicents).
The result is a list of chord-sequences that can be fed into the 'chord-seq'input of a multi-sequence object. Each chord-seq is a possible solution, sorted from the least significant to the most significant. Each chord-seq is a series of chords where the lowest note is a virtual fundamental and the remaining notes are a subset
of the original chord.
<thresh> is a minimum pitch/frequency value for the virtual fundamentals. If the menu <unit> is 'freq' then <chord> and <thresh> must be given in Hz, otherwise in Midics."

   (multi-virt-fun chord approx thresh unit))

(defmethod! virt-fund-multi ((chord chord) (approx integer)  (thresh number)  (unit symbol))

  (multi-virt-fun chord approx thresh unit))

;================================================================================


; --------------------- traitement de spectres (spectra menu)---------------------


(om::defmethod! traite-ampl ((spectre list) (max number) (filtre% number))
  :initvals '('((1 2 3 4 5) (10 9 5 3.5 1.7))  100 20)
  :indoc '("Spectrum (ranks ampl)" "New max ampl" "Ampl low limit")
  :icon 136
  :doc "met les intensités du spectre à l'échelle donnée par <max> et retire les composantes 
dont l'amplitude est < à % donné par filtre%"
  (let  ((res (multi-fil   #'< (* (list-max (nth 1 spectre)) (/ filtre% 100.0)) spectre 1)))
    (list (nth 0 res) (scaling/max  (nth 1 res) max))))


;====Rename traite-ampl as treat-amplitude 25-06-2007===================

(om::defmethod! treat-ampl ((spectre list) (max number) (filtre% number))
  :initvals '('((1 2 3 4 5) (10 9 5 3.5 1.7))  100 20)
  :indoc '("Spectrum (ranks ampl)" "New max ampl" "Ampl low limit")
  :icon 136
  :doc "Scales the intensities (obtained after the spectral analysis) with respect to the maximum value <max> and removes those components with an amplitude below a the percetage specified at <filtre%>."

  (traite-ampl spectre max filtre%))

;======================================================================

(om::defmethod! midi-ampl ((spectre list) (velmin number))
  :initvals '('((1 2 3 4 5) (10 9 5 3.5 1.7))  70)
  :indoc '("Spectrum (ranks ampl)" "Ampl low limit")
  :icon 136
  :doc
  "Scales the intensities (obtained after the spectral analysis) with respect to the maximum value equal to 127, adjusts for a MIDI transfer through function (14.044 * x ^ 0455) and removes those components with a velocity below the specified <velmin>."
  (let (resul)
    (setq resul (om-round  (mapcar #'(lambda (x) (* (expt x .455) 14.044)) 
                                   (scaling/max (nth 1 spectre) 127)) 1) ) 
    (multi-fil #'< velmin (list (nth 0 spectre) resul ) 1)))

; =================== conversions ======================================



;---------------------conversions temporelles--------------------

(defun sec->min1 (sec nbdec format)
(let ((min (truncate sec 60)))
  (if  (and (equal format 'abbrev )(= 0 min)) (list(om-round sec nbdec))
      (list min 'min (om-round (mod sec 60) nbdec)))))


(om::defmethod! sec->min ((lsec t ) &optional (nbdec 2 ) (format 'normal))
  :initvals '(68 2 'normal)
  :indoc '("lsec"  "nbdec" "format" )
  :menuins '((2 (("normal" 'normal) ("abbrev"'abbrev))))
  :icon 137
  :doc "Converts values in seconds (<lsec>)to values in minutes and seconds. The 
optional argument <nbdec> determines the number of decimals in the 
seconds column of the output. 
The output is in the format '1 min 15' for an <lsec> equal to '75'. If the 
number of seconds is less than sixty the output will be in the form 
'0 min 32'. The optional argument <format>, if set to the position 
'abbrev', will eliminate the minutes column if it has a value of '0'. 
(The first example would remain '1 min 15' while the second would become  '32')"
  (deep-mapcar/1  'sec->min1 lsec nbdec format))



(defun min->sec1 (minutage) 
  (let ((sec 0) (minutes 0) (minutage (list! minutage)))
       (cond  ((= (length minutage) 1)  (setq sec (car minutage)))
              (( or (numberp (second minutage)) (= (length minutage) 3))
                             (setq minutes (car minutage)) (setq sec (car (last minutage))))
              (t (setq minutes (car minutage))))
       (om-round (+  sec (* minutes 60)) 2)))


(om::defmethod! min->sec ((minutes list ))
  :initvals '('(1 min 30) )
  :indoc '("minutes" )
  :icon 136
  :doc "Converts values in minutes into values in seconds. The value in minutes
may be entered as a list in any of the following formats: (3 min), or 
(3 0); (3 min 30), or (3 30), or (3.5 min); (3 min 30.2), or (3 30.2). 
(the letters 'min' may be replaced by simply 'm' or any other non-numeric 
character or characters) "
  (less-deep-mapcar  'min->sec1 (list! minutes)))



(om::defmethod! addtime ((temps list) )
  :initvals '('((1 min 30) (2 min 12) ))
  :indoc '("temps" )
  :icon 136
  :doc  "additionne des minutages donnés sous forme de liste de liste
Minutages négatifs possibles; orthographe : (-1 m 5)"
  (sec->min (apply #'+  (min->sec temps)) 0))


(om::defmethod! intertime ((temps list) )
  :initvals '('((1 min 30) (2 min 12) ))
  :indoc '("temps" )
  :icon 136
  :doc    "donne intervalles entre minutages donnés sous forme de liste de liste"
  (sec->min (x->dx  (min->sec temps)) 0 ))


(om::defmethod! cumultime ((tdeb list) (durs list ))
  :initvals '('(0 min 45) '( 30 12) )
  :indoc '("tdeb" "durs" )
  :icon 136
  :doc    "cumuls successifs d'une liste de durées (en secondes), à partir
d'un temps de départ 'tdeb' (en minutes-secondes) " 
  (rest (sec->min (dx->x  (min->sec tdeb) durs) 2)))



(om::defmethod! pro-max ((accord chord) (approx integer ) (canal integer))
  :initvals '(t 4 1)
  :indoc '("chord" "approx" "canal")
  :icon 136
  :doc   "construit une liste au format max  note vel canal "


  (let ((hauteurs (lmidic accord))
        (velos    (lvel accord )))
    (while hauteurs (let ((hauteur (approx-m (nextl hauteurs) approx)))
                     (format t "~A ~A ~A \," (truncate hauteur 100) (nextl velos) 
                           (+  canal (/ (mod  hauteur 100) 25)))))
    (format t "~%")
    nil))



(om::defmethod! pro-max-dur ((accord chord) (approx integer ) (canal integer))
  :initvals '(t 4 1)
  :indoc '("chord" "approx" "canal")
  :icon 136
  :doc   "construit une liste au format max  note vel canal off dur "

  (let ((hauteurs (lmidic accord ))
        (velos    (lvel accord))
        (durees (ldur accord))
        (offsets (loffset accord)))
    (while hauteurs (let ((hauteur (approx-m (nextl hauteurs) approx)))
                     (format t "~A ~A ~A ~A ~A \," (truncate hauteur 100) (nextl velos) 
                           (+  canal (/ (mod  hauteur 100) 25))
                           (* 10 (nextl offsets)) (* 10 (nextl durees)))))
    (format t "~%")
    nil))
; ------------ approximations


(defun best-micro/1 (accord nbmicro approx)
  (let* ( (ldist (om-modulo accord 100)) (app (/ 200 approx)) 
         (ldistapp nil)  (ldist2 (copy-list ldist)) res )

    (while ldist2 (let ((item (nextl ldist2)))
                   (if (< item app) (push (- app item) ldistapp) 
                                    (push (- item app) ldistapp))))
    
    (setq ldistapp (reverse ldistapp))
    (setq ldist2 (sort-list (copy-list ldistapp)))
    
    (let ((mini (l-nth ldist2 (1- nbmicro))))
      
      (while ldistapp  (let ((item (nextl ldistapp)))
                         (if (<= item mini)
                            (progn (push app res)  (nextl ldist))
                            (push (om* (* app 2) (ll/round (nextl ldist) (* app 2))) res )))))
    
    (om+ (nreverse res) (om* 100 (om-floor accord 100)))))






(om::defmethod! best-micro ((accords list) (nbmicro integer) (approx integer))
  :initvals '('(6000 6345 6625) 1 4)
  :indoc '("accords" "nbmicro" "approx" )
  :icon 136
  :doc   "ne rend l'approx <approx> que pour les <nbmicro> hauteurs les mieux placées
Si <nbmicro> = 0, rend l'accord approximé normalement"
  (if (or (< approx 3) (= nbmicro 0)) (approx-m accords approx)
          (less-deep-mapcar  #'best-micro/1 accords nbmicro approx)))



(defun filtre-micro/1 (accord approx  crible )
  (let ((modulo (/ 200 crible)) (accord (approx-m accord approx)) res)
    (dolist (n accord)
      (if (= (mod n modulo) 0) (push n res)))
    (nreverse res)))

(om::defmethod! filtre-micro ((accords list) (approx integer) (crible integer))
  :initvals '('(6000 6345 6625 6987) 4 2)
  :indoc '("accords"  "approx" "crible")
  :icon 136
  :doc  "approxime l'accord, puis enlËve les sons n'appartenant pas à l'ensemble 
des sons en 1/2 ou 1/4, selon <crible> "
  (less-deep-mapcar #'filtre-micro/1 accords approx crible))


;; ---- midic -> symbol ----

(om::defmethod! ratio->cents ((coef number))
  :initvals '(1.05946)
  :indoc '("Freq ratio")
  :icon 136
  :doc "<coef->cents> takes a frequency ratio <coef> f1/f2 and returns the interval, 
expressed in midi-cents, between f1 and f2."

  (round (log coef) #.(/ (log 2) 1200)))


(om::defmethod! cents->ratio ((nb-cents number)) 
  :initvals '(100)
  :indoc '("Cents")
  :icon 136 
  :doc  "<cents->coef> takes an interval expressed in midi-cents and returns the ratio 
between two frequencies separated by that interval; i.e., the value: (freq + <nb-
cents>) / freq."

  (expt 2.0 (/ nb-cents 1200.0)))



(om::defmethod! inter->freq ((accord list))
  :initvals '('(2400 3100 4050))
  :indoc '("accord")
  :icon 136 
  :doc  "intervalles en fréquence entre chaque note de l'accord"
  (om-abs (x->dx (mc->f accord))))


(om::defmethod! diff->dist ((fond number) (rang number) (diff number) )
  :initvals '(2400 5 235)
  :indoc '("fond" "rang" "diff")
  :icon 136 
  :doc  "convertit une distorsion exprimée en midic par rapport à un rang en 
distorsion exprimée en %"
  (let ((f (mc->f fond)) (d (mc->f diff)))
  (/ (log (/ (+ d (* f rang)) f)) (log rang))))


(defun lin->db1 (amp) 
  (if (zerop amp) -3.63224978306E9
      (* 20.0 (log amp 10))))


(om::defmethod! tm-lin->db ((amps list) &optional (nbdec 2))
  :initvals '('(1024 2048) 2)
  :indoc '("Amplitudes" "Nb decimals")
  :icon 136
  :doc "<tm-lin->db> takes a  number <amps> and returns the corresponding value 
expressed in decibels. The input can be a list of numbers. In this case a list of 
db values is returned."

  (om-round (deep-mapcar/1 'lin->db1 amps) nbdec))



(defun dB->lin1 (amp) (expt 10.0 (/ amp 20.0)))


(om::defmethod! tm-db->lin ((amps list)  &optional (nbdec 0)) 
  :initvals '('(60 66) 0)
  :indoc '("Decibels" "Nb decimals")
  :icon 136
  :doc  "<tm-db->lin> takes a  number <amps> in decibels and converts it
to linear. The input can be a list of numbers. In this case a list of 
linear values is returned."

  (om-round (deep-mapcar/1  'db->lin1 amps) nbdec))


(defun choisir-separateurs (texte separateurs ) 
  (let ((lchar (coerce texte 'list)) (separateurs (list! separateurs)))
    (dolist (sep separateurs)
      (setq lchar (ll-replace lchar sep #\Space) ))
    (coerce lchar 'string)))

(defun m->can1 (chord approx)
  (let* ((res (clone chord))
        (l-midics (approx-m (lmidic) approx))
        (l-canaux (lchan res)))
    (setf  (lchan res)  (om+  l-canaux (om/ (second (multiple-value-list  (om// l-midics 100)))  25)))
    (setf (lmidic res) (om* 100 (first (multiple-value-list  (om// (om/ l-midics 100) 1)))))
    res))
        

(om::defmethod! midic->canal ((chords t) (approx integer))
  :initvals '(t 4 )
  :indoc '("chords" "approx")
  :icon 136
  :doc  "rend chord approximé où l'indication de 1/4 de ton ou de 1/8 ton est fournie
par le canal (+ 1 2 3 selon micro-int)"
        (car-mapcar #'m->can1  (lmidic chords) approx))



; changement de port et de canal

(om::defmethod! map-channel ((obj chord) (mapping list))
  :initvals '(nil (((0 1) (1  3))))
  :indoc '("chord" "mapping list")
  :icon 136
  :doc  "gives new port and channel numbers . Format of mapping list :
(((oldport oldchannel) (newport newchannel)) ((oldport oldchannel) (newport newchannel))...)
ex :  ( ((0 1) (3 5))  ((1 3) (1 7))  ((1 9) (2 11)) )
if one change only, don't forget outer brackets : ( ((0 1) (3 5)) ) "
  
  (let ((ports (lport obj)) (chans (lchan obj))  )
    (for  (i 0 1 (length-1 ports))
      (loop for m in mapping
            
            do (if   (and (= (nth i ports) (car (car m))) (= (nth i chans) (second (car m))))
                 (progn (setf (nth i ports)(car (second m)))
                        (setf (nth i chans)(second (second m)))))))
    (mki 'chord
         :LMidic (lmidic obj)
         :Lvel (lvel obj)
         :Loffset (loffset obj)
         :Ldur (ldur obj)
         :Lchan chans
         :Lport ports)))



(om::defmethod! map-channel ((obj chord-seq) (mapping list))
(mki 'chord-seq
     :lmidic (loop for ch in (chords obj)
                       collect (map-channel ch mapping))
     :lonset (lonset obj)
     :legato (legato obj)))



(om::defmethod! map-channel ((obj multi-seq) (mapping list))
   (mki 'multi-seq
        :chord-seqs (loop for chseq in (chord-seqs obj)
                          collect (map-channel chseq mapping))))






;=============================CONTROL==================================



(om::defmethod! aiguillage ((num integer) (pat1 list)
                               (pat2 list)
                               &rest  patches)
  :initvals '(1 '() '() '())
  :indoc '("num" "pat1" "pat2" "patches")
  :icon 137
  :doc  "déclenche le patch branché à l'entrée indiquée par <num>"

  (let ((lpatches (x-append (list pat1 pat2 ) patches)))
    (posn-match lpatches (1- num))))



(om::defmethod! lambda-mapcar ((fct t ) (liste list) (arg t))
  :initvals '('+ '() '())
  :indoc '("fct" "liste" "arg")
  :icon 136
  :doc  ""

  (mapcar #'(lambda (x) (funcall fct x arg) ) liste))




; ================= ensembles =======================================


(om::defmethod! unique-notes ((acc1 t) (approx integer)  &rest accords )

  :initvals '('(6000 6600 6615) 4 () )
  :indoc '("Midics" "Approx" "Midics")
  :icon 137
  :doc "retire notes redoublées, aprËs approximation.
Accepte listes d'accords sur l'entrée acc1, ou avec extensions, différents
accords sur chaque entrée"
  (remove 0 (less-deep-mapcar 'unique  (append (approx-m acc1 approx)
                                     (flat (deep-mapcar/1 'approx-m  accords approx ))))))


(om::defmethod! common-notes ((accord1 list)  (approx integer) (accord2 list) &rest accords )
                        
  :initvals '('(6000 6600) 4 '(6200 6600) () )
  :indoc '("Midics" "Approx" "Midics" "Midics")
  :icon 137
  :doc  "notes communes, aprËs approximation"
  (let ((ll (intersection (approx-m accord1 approx) (approx-m accord2 approx))))
    (while accords
      (setq ll (intersection  (approx-m (pop accords) approx) ll)) )
    ll))


(om::defmethod! notes-union ((accord1 list)  (approx integer) (accord2 list) &rest accords )

  :initvals '('(6000 6600) 4 '(6200 7000) () )
  :indoc '("Midics" "Approx" "Midics" "Midics")
  :icon 137
  :doc  "union des notes des deux accords, sans répétition, selon approx. 
L'accord rendu est approximé"
  (let ((ll (x-union (approx-m accord1 approx) (approx-m accord2 approx))))
    (while accords
      (setq ll (x-union ll  (approx-m (pop accords) approx)) ))
    (sort-list ll '<)))


(om::defmethod! notes-libres ((accord list) (approx integer)  &rest accords )
  :initvals '('(6000 6600 7300 8850) 4  () )
  :indoc '("Midics" "Approx" "Midics" )
  :icon 137
  :doc  "degrés non utilisés par un accord ou une série d'accords, selon approx"
  (let ((gamme (arithm-ser 6000  7199 (/ 200 approx))) 
        (ll (transpoct (approx-m accord approx) 6000 7199)))
    (while accords
      (setq ll (x-union ll (transpoct (approx-m (pop accords) approx) 6000 7199) )))
     (x-diff gamme ll)))



(om::defmethod! notes-libres ((accord chord) (approx integer)  &rest accords )
  (notes-libres (lmidic accord) approx))






; ======================= intervals ==========================



(defun max-abs-idt (ch1 ch2)
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (mapcar #'- ch1 ch2))
         (int-min (apply #'min ints))
         (int-max (apply #'max ints)))
    (values (/ (- int-max int-min) 2) (/ (+ int-max int-min) 2))))

;;so that user extended box works with max-abs-idt as default value...!!!
(defun CL-USER::max-abs-idt (ch1 ch2) (max-abs-idt ch1 ch2))

(defun ma-best-transp (ch1 ch2) 
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (max-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))


;; - the best transposition "Tbest" is the middle point of the list:
;;   (cond ((oddp (length ints)) (nth (/ n 2) ints))
;;         ((evenp (length ints)) (/ (+ (nth (floor n 2) ints)
;;                                     (nth (floor (1+ n) 2) ints)) 2)))
;; - the corresponding distance is:
;;   D = (Sum (i 0 n/2) |Tbest-INTi|) + (Sum (i n/2 n+1/2) |INTi-Tbest|)
;;   D = (Sum (i n/2 n+1/2) INTi) - (Sum (i 0 n/2) INTi)

(defun sum-abs-idt (ch1 ch2)
  "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (sort (mapcar #'- ch1 ch2) #'<))
         (1-length (1- (length ints)))
         (summin 0) (summax 0)
         transpos)
    (repeat (floor 1-length 2)
      (incf summin (nextl ints)))
    (if (evenp 1-length)
      (nextl ints transpos)
      (progn
        (incf summin (nextl ints transpos))
        (setq transpos (/ (+ transpos (car ints)) 2))))
    (while ints (incf summax (nextl ints)))
    (values (- summax summin) transpos)))

(defun sa-best-transp (ch1 ch2) 
  "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (sum-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))

(defmethod! besttransp ((ch1 list) (ch2 list) (fct symbol))
  :initvals '('(6000) '(6000) 'sum)
  :indoc '("chord" "chord" "fct")
  :menuins '((2 (("Sum" 'sum) ("Max" 'max)))  )
  :icon 136 
  :doc     
 
"Transposes the chord <ch2> (a single chord in midicents) so that its 
intervallic distance to <ch1> (also a single chord in midicents) is as 
small as possible. Thus the distance between each note of <ch2> and each 
note of <ch1> becomes as small as possible.This is essentially the same 
as the box 'best-inv' except the ordering of <ch2> is preserved.

The optional argument <fct> allows the choice between two different 
algorithms for calculating this function,'sum' and 'max'. The default
is sum because 'max' may produce quarter-tones from semi-tone input. For
best results one should experiment with both and chose according to the 
context."

(om+ ch2 (if (eq fct 'max) (ma-best-transp ch1 ch2) (sa-best-transp ch1 ch2))))


(defmethod! besttransp ((ch1 t) (ch2 list) (fct symbol))
   (besttransp (lmidic ch1) ch2 fct))

(defmethod! besttransp ((ch1 list) (ch2 chord) (fct symbol))
   (besttransp  ch1 (lmidic ch2) fct))

(defmethod! besttransp ((ch1 chord) (ch2 chord) (fct symbol))
   (besttransp (lmidic ch1) (lmidic ch2) fct))

;===Rename besttransp as closest-trans 25-06-2007==========

(defmethod! closest-trans ((ch1 list) (ch2 list) (fct symbol))
  :initvals '('(6000)  '(6000) 'sum)
  :indoc '("chord" "chord" "fct")
  :menuins '((2 (("Sum" 'sum) ("Max" 'max)))  )
  :icon 136 
  :doc
  "Transposes the chord <ch2> (a single chord in midicents) so that its 
intervallic distance to <ch1> (also a single chord in midicents) is as 
small as possible. Thus the distance between each note of <ch2> and each 
note of <ch1> will become as small as possible. This is essentially the same 
as the box 'best-inv' except the ordering of <ch2> is preserved.

The optional argument <fct> allows the choice between two different 
algorithms for calculating this function,'sum' and 'max'. The default
is sum because 'max' may produce quarter-tones from semi-tone input. For
best results one should experiment with both and chose according to the 
context."

  (besttransp ch1 ch2 fct))

(om::defmethod! closest-trans ((ch1 list) (ch2 chord) (fct symbol))
  (make-instance 'chord
    :lmidic
    (closest-trans ch1 (lmidic ch2) fct)
    :ldur (ldur ch2)
    :lvel (lvel ch2)
    :loffset (loffset ch2)
    :lchan (lchan ch2)))

(om::defmethod! closest-trans ((ch1 chord) (ch2 list) (fct symbol))
  (make-instance 'chord
    :lmidic
    (closest-trans (lmidic ch1) ch2 fct)
    :ldur (ldur ch1)
    :lvel (lvel ch1)
    :loffset (loffset ch1)
    :lchan (lchan ch1)))

(om::defmethod! closest-trans ((ch1 chord) (ch2 chord) (fct symbol))
  (make-instance 'chord
    :lmidic
    (closest-trans  (lmidic ch1) (lmidic ch2) fct)
    :ldur (ldur ch2)
    :lvel (lvel ch2)
    :loffset (loffset ch2)
    :lchan (lchan ch2)))

;===============================================================



(defun transpoct-prox1 (midics min max  pivot )
  ""
  (let ((mmin (max min (- pivot 1200)))
        (mmax (min max (+ pivot 1200))))
    (cond
     ((> mmin mmax)
      (error "The intervals [~S ~S] [~S-1200 ~S+1200] don't intersect."
             min max pivot pivot))
     ((< (- mmax mmin) 1200)
      (error "The intersection between [~S ~S] and [~S-1200 ~S+1200] is smaller than one octave."
             min max pivot pivot)))
    (transpoct midics mmin mmax)))



(defun transpoct-prox (chord  min  max  pivot)
  "Transposes <chord> (midics) by octaves to fit into the interval [<min> <max>]
while making with <pivot> an interval smaller than one octave.
<Chord> may be a list of chords"
  (less-deep-mapcar 'transpoct-prox1 chord min max pivot))

(defun transpoct1 (midics min max)
  ""
  (let ((result (mapcar #'(lambda (midic)
                            (while (< midic min) (incf midic 1200))
                            (while (> midic max) (decf midic 1200))
                            midic) (list! midics))))
    (if (cdr result) result (car result))))


(om::defmethod! transpoct ((chord list) (min integer) (max integer) &optional (pivot  0))

  :initvals '('(5500 6600 7850) 6000 7200 0 )
  :indoc '("Midics" "Min" "Max" "Pivot")
  :icon 137
  :doc   "Transposes notes of a chord or list of chords <chord> by octaves such 
that all its notes will be contained within the range between <min> and
<max>, given in midicents.

The optional argument <pivot>(a note, in midicents) forces all notes 
to be transposed so that they will be within one octave of that note. 
<Pivot> must be within the specified range, or an error will be 
produced."
  (if (< max min) (rotatef max min))
  (if (zerop pivot)
    (less-deep-mapcar 'transpoct1 (list! chord) min max)
    (less-deep-mapcar 'transpoct-prox1 (list! chord) min max pivot)))


;===Rename transpoct as oct-trans 25-06-2007===================== 

(om::defmethod! oct-trans ((chord list) (min integer) (max integer) &optional (pivot  0))

  :initvals '('(5500 6600 7850) 6000 7200 0 )
  :indoc '("Midics" "Min" "Max" "Pivot")
  :icon 137
  :doc   "Transposes notes of a chord or list of chords <chord> by octaves so all its notes will be contained within the range between <min> and <max>, given in midicents.

The optional argument <pivot> (a note, in midicents) forces all notes to be transposed to the same octave as the pivot. The <pivot> must be within the specified range, or an error will occur."

  (transpoct chord min max pivot))


(om::defmethod! oct-trans ((self chord) (min integer) (max integer) &optional (pivot  0))
  (make-instance 'chord
    :lmidic
    (oct-trans  (lmidic self) min max)
    :ldur (ldur self)
    :lvel (lvel self)
    :loffset (loffset self)
    :lchan (lchan self)))


(om::defmethod! oct-trans ((self chord-seq) (min integer) (max integer) &optional (pivot  0))
  (make-instance 'chord-seq
    :lmidic
    (oct-trans (mapcar 'lmidic (chords self)) min max)
    :lonset (lonset self)
    :ldur (ldur self)
    :lvel (lvel self)
    :loffset (loffset self)
    :lchan (lchan self)
    :legato (legato self)))



(om::defmethod! oct-trans ((self multi-seq) (min integer) (max integer) &optional (pivot  0))
  (make-instance 'multi-seq 
    :chord-seqs 
    (loop for chord-seq in  (chord-seqs self)
          collect 
          (oct-trans chord-seq toler))))


;=================================================================


(om::defmethod! mul-chord ((ch1 list)  (ch2 list) &optional (type 'chord))          

  :initvals '('(6000 6300) '(6400 6700)  'chord ) 
  :icon 137
  :doc   "Generates a list of chords in which the intervallic structure of <ch2>
(a single chord in midicents) is reproduced beginning on each successive 
note of <ch1> (also a single chord in midicents).
The optional argument <type> allows the choice of whether the output is
a list of chords ('seq') or a single chord ('chord') containing all the 
transpositions combined."

   (let ((ch1 (list! ch1)) (int2 ()) (base-note-2 (apply 'min ch2)) res)
      (while ch2 (newl int2 (- (nextl ch2) base-note-2)))
      (setq int2 (nreverse int2))
      (setq res (mapcar 
        #'(lambda (midic) (mapcar #'(lambda (iv) (+ iv midic)) int2))
        ch1 ))
      (if (eq type 'chord) (flat res) res )))


;=========Rename mul-chord as chord-multiplier 24-06-2007=========================

(om::defmethod! chord-multiplier ((ch1 list)  (ch2 list) &optional (type 'chord))          

  :initvals '('(6000 6300) '(6400 6700)  'chord ) 
  :icon 137
  :doc   "Generates a list of chords in which the intervallic structure of <ch2> (a single chord of pitches as a list in midicents) is reproduced beginning on each successive note of <ch1> (also a single chord in midicents). The optional argument <type> allows the choice of whether the output is a list of chords ('seq') or a single chord ('chord') containing all the transpositions combined."
  
  (mul-chord ch1 ch2 type))


; ================================ traitement de listes ===========================

; ............  utilitaires

(defun nnth (arg list ) 
  (nth arg (flat list)))

(defun aplatit (list listmem)
  (cond ((atom list) list)
        ((atom (car list)) list)
        ((atom (car (car list))) (flat-once  listmem)) 
        (t (aplatit (car list) listmem))))

(defun supprimelem (liste elem )
  (let ((long (1- (length liste))) result)
    (for (i 0 1 long)
         (if (= i elem) (progn (pop liste) (push () result))
             (push (pop liste) result)))
    (remove () (nreverse result))))

;--------------------------------------------------------

(om::defmethod! lister ((list1 t) (list2 t) &rest lst?)
   :initvals '('(1 2) '(1 2) '(1 2))
   :indoc '("list1" "list2" "lst?")
   :icon 137
   :doc  "puts lists together"   
   (x-append (list list1 list2)  lst?))


(om::defmethod! lister4 ((list1 t) (list2 t) (list3 t) (list4 t)
                            &rest lst?)
   :initvals '('(1 2) '(1 2) '(1 2) '(1 2) '(1 2) )
   :indoc '("list1" "list2" "list3" "list4" "lst?")
   :icon 137
   :doc  "puts lists together" 
   (x-append (list list1 list2 list3 list4)  lst?))




;-------------------------- extraction ------------------------------


(om::defmethod! atom! ((data t))
  :initvals '('(1 2 3 4 5) )
  :indoc '("data")
  :icon 136
  :doc  "Works similarly as 'first', but also accepts an atom at the input."

  (if (atom data) data (first data)))

(defun deuxieme (liste) ""
  (second liste))

(defun troisieme (liste) ""
  (third liste))


(om::defmethod! list-pos ((liste list) (deb number) (fin number)
                 &optional (niveau 'high))

   :initvals (list '(1 2) 0 1 'haut)
   :indoc '("liste" "deb" "fin" "niveau" )
   :icon 137
   :menuins '((3 (("high" 'high) ("med" 'med) ("low" 'low) )))
   :doc "Extracts at the specified positions between beginning and end. Options: 'high' means operation occurs hierarchically at the highest sublists.'med' means operation occurs at the second level. 'low' means the operation takes place at the lowest sublists." 


  (let* ((indi (if (< fin  deb) -1 1)) )
    (remove nil  
            (cond ((equal niveau 'high) (l-nth liste (arithm-ser deb fin indi) ))
                  ((equal niveau 'med) (car-mapcar  #'l-nth liste (arithm-ser deb fin indi)))
                  ((equal niveau 'low) (less-deep-mapcar  #'l-nth liste (arithm-ser deb fin indi)))
                  ))))
    
#|
; pour compatibilité
(defunp liste-pos ((liste list) (deb fix) (fin fix)) list
  ""                 
  (list-pos liste deb fin 2))
|#

(om::defmethod! guillotine ((liste list))
  :initvals '(  '((1 2 3 4 5) (1 6 7 8 9)))
  :indoc '("List of lists")
  :icon 136
  :doc  "Returns the first elemement of every sublist. The input may be a list or a list of lists."
  
(let ((end (length liste)) listinter sousliste)
    (dotimes (n end)
      (setq sousliste (cdr (nth n liste)))
      (push sousliste listinter))
    (setq liste (reverse listinter))))

(om::defmethod! l-extract ((texte t) (ncol integer) (select t))
  :initvals '(  '(1 100 3.0 2 200 5.0 3 300 2.7 ) 3 '(1 2))
  :indoc '("Raw text" "Number of columns" "Choice of columns (list)")
  :icon 136
  :doc  "Organizes a raw data list (for example a spectral analysis file). Applies a list-modulo in <ncol> columns. Selects the columns indicated by <select> and returns a list of lists."
  
(l-nth  (list-part texte ncol) (list! select)))


;====Rename l-extract as organizer 26-06-2007==============

(om::defmethod! organizer ((text t) (ncol integer) (select t))
  :initvals '(  '(1 100 3.0 2 200 5.0 3 300 2.7 ) 3 '(1 2))
  :indoc '("Raw text" "Number of columns" "Choice of columns (list)")
  :icon 136
  :doc  "Organizes a raw data list (for example a spectral analysis file). Applies a list-modulo in <ncol> columns. Selects the columns indicated by <select> and returns a list of lists."
  
  (l-extract text ncol select))

;==========================================================

(om::defmethod! penult ((liste list))
  :initvals '(  1 2 3 4 5)
  :indoc '("List")
  :icon 136
  :doc  "Extracts the 'butlast' element from a list. For example from the list (0 1 (2 10) 3 (4 1) 5 6 7) it returns 6."
 
(last-elem (butlast liste)))

;-------------------------- liste-analyse  ------------------------------

(defun l-sum1 (liste)
  (apply #'+ liste))

(om::defmethod! l-sum ((liste list))
  :initvals '(  '(1 2 3 4 5 6 7 ) )
  :indoc '("liste")
  :icon 136
  :doc  "Returns the total sum of all elements of a list."
  (less-deep-mapcar #'l-sum1 liste))

(defun positions1 (list elem )
  (let ((index 0) res)
    (dolist (n list)
      (if (eq elem  n )  (push index res)) 
      (setq index (1+ index)))
    (nreverse res)))

(om::defmethod! positions ((list list) (elem t))
  :initvals '(  '(1 2 3 1 5 1 7 ) 1 )
  :indoc '("list" "elem")
  :icon 136
  :doc  "Returns all positions of a certain element within a list."
  (less-deep-mapcar  'positions1 list elem))

(defun nbi-rec (liste res)
  (let ((sublist ()))
  (push (length (positions liste (first liste))) sublist)
  (push (first liste) sublist)
  (push sublist res)
  (setq liste (remove (first liste) liste))
  (if (not (null liste)) (nbi-rec liste res) res)))

(om::defmethod! nbelem-ident ((liste list))
  :initvals '(  '(a b a c b a d e ) )
  :indoc '("liste")
  :icon 136
  :doc "donne le nombre d'éléments identiques d'une liste, rangés par 
ordre de fréquence décroissante
sortie: liste elems, liste frequences "
   (let ((res (sort-table  (mat-trans (nbi-rec liste () )) 1)))
     (list (nreverse (first res)) (nreverse (second res)))))

;====Rename nbelem-ident as n-occur 28-06-2007==================

(om::defmethod! n-occur ((liste list))
  :initvals '('(a b a c b a d e))
  :indoc '("liste")
  :icon 136
  :doc "Returns the number of indentical elements within a list, ordered by decreasing frequency (i.e. number of occurences).

Output: list of identical elements, list of frequencies."

  (nbelem-ident liste))

;===============================================================

(om::defmethod! length-1 ((list list))
  :initvals '('(1 2 3 4 5 6 7))
  :indoc '("list")
  :icon 136
  (1- (length list)))


;-------------------------- substit/insert  ------------------------------


(defun rec-suppress (liste elem)
  (while elem
      (setq liste  (rec-suppress (suppress-one liste (pop elem)) (cdr elem))))
    liste)

(defun suppress-one (liste elem )
  (let ((long (1- (length liste))) result)
    (for (i 0 1 long)
         (if (= i elem) (progn (pop liste) (push () result))
             (push (pop liste) result)))
    (nreverse result)))

(om::defmethod! l-suppress ((liste list) (elem t))
  :initvals '(  '((1 2 3 4 5) 0) 0)
  :indoc '("List of lists" "elem")
  :icon 136
  :doc  " retire les éléments de numéro <elem> (peut être une liste) de la liste"
  (remove () (rec-suppress liste (list! elem))))

;(l-suppress '(1 2 3 4  5) '(0 2))


(defun remove-all (list nums)
  (let ((count 0))
    (dolist (item nums list)
      (setf (nthcdr (- item count) list) (nthcdr (+ item 1 (- count)) list))
      (incf count))))

(defun get-useful-nums (nums length)
  (let ((elems (unique (list! nums))) list)
    (dolist (a-num elems (nreverse list))
      (cond ((minusp a-num) )
            ((< a-num length) (push a-num list))
            (t (return (nreverse list)))))))

; corr. 8-6-03
(defun multi-fil (test val list numcol )
"retire de chacune des sous-listes de <list> les éléments dont le numéro d'ordre
correspond à chaque élément de la sous-liste de numéro <numcol> qui satisfait
à la condition <test val>"
  (if (atom (car list)) (filtre-liste test val list)
      (let ((longueur (length (nth numcol list))) 
            (ncol (1- (length list)))  res)
        (dotimes (n longueur)
          (if (funcall test  (car (nth numcol list)) val)
            ()
            (for (i 0 1 ncol)
             (push (car (nth i list)) res)))
          (setq list (ll-suppress list 0)) )
        (if (null res) () (list-modulo (reverse res) (1+ ncol))) )))

(defun l-delete (list elem  )
"deletes the elemth (can be a list) element from list. If <elem> is a list of
numbers, these have to be ordered "
  (let ((numbers (get-useful-nums elem (length list))))
    (if numbers 
      (let ((save-l (copy-list list)))
         (if (zerop (car numbers))
           (cdr (remove-all save-l (cdr numbers)))
           (remove-all save-l numbers)))
      list)))


(om::defmethod! ll-suppress ((lliste list) (elem t ))
  :initvals '(  '((1 2 3 4 5) (1 6 7 8 9)) '(0 3))
  :indoc '("List of lists" "Num of elements")
  :icon 136
  :doc   " retire les éléments de numéro <elem> de chaque sous-liste de la liste"
  (mapcar #'(lambda (x) (l-delete x elem)) lliste))




(om::defmethod! ll-remove ((list list) (item t))
  :initvals '(  '((1 2 3 4 5) (3 3 7 8 9)) 3)
  :indoc '("List of lists" "Element")
  :icon 136
  :doc  "remove sur listes de listes "
  (less-deep-mapcar  #'(lambda (x) (remove item x))   list ))


(om::defmethod! ll-replace ((list list) (old t) (new t))
  :initvals '(  '((1 2 3 4 5) (3 3 7 8 9)) 3 'b)
  :indoc '("List of lists" "Element to replace" "New element")
  :icon 136
  :doc  "replace on list of lists "
  (less-deep-mapcar  #'(lambda (x) (substitute new old  x))   list ))



(defun insert1 (liste insert pos )
  (let ((long (1- (length liste))) res )
    (if (> pos 0) 
      (dolist (d (list-pos liste 0 (1- pos)))
      (push d res)))
    (if (consp insert)
      (dolist (d insert)  (push d res))
      (push insert res))
    (if (<= pos long)
      (dolist (d (list-pos liste pos long))
        (push d res)))
    (nreverse res)))

(defun insert-rec (liste insert pos )
  (while (not (null  pos))
    (setf liste (insert1 liste (pop insert) (pop pos) ))
    (insert-rec liste insert  pos ))
  liste)
     
(om::defmethod! ll-insert ((liste list)  (insert  t ) (pos integer))
  :initvals '(  '(1 2 3 4 5)  'f 2)
  :indoc '( "liste" "insert" "pos")
  :icon 136
  :doc  " insËre une liste ou un élément au sein de la liste <liste> à partir de la position <pos>
On peut insérer une liste d'insertions, avec les positions correspondantes dans pos
Jouer avec les parenthËses pour obtenir l'effet de niveau désiré
Nouvelle version 28/7/96"
  (if (one-elem pos)
    (insert1 liste insert pos)
    (insert-rec liste insert pos)))


; gardé pour compatibilité
(defun l-insert (liste  insert pos ) 
" insére une liste ou un élément au sein d'un autre à partir
de la position pos"
  (x-append (liste-pos liste 0 (1- pos)) insert (liste-pos liste  pos (1- (length liste)))))


(defun substit-one (liste  elem val fct)
  (let ((long (1- (length liste) )) (val (list val)))
    (x-append (l-nth liste (arithm-ser 0  (1- elem) 1))
          (if (equal fct '=) val (funcall fct (l-nth liste elem) val ))
          (l-nth liste (arithm-ser (1+ elem) long 1)) )))

(om::defmethod! substit ((liste list) (elem t) (val t)  &optional (fct  '=))
   :initvals '(  '(1 2 3 4 5) '(0 3) '(12 10) '=)
   :indoc '( "liste" "elem" "val" "fonction")
   :icon 137
   :doc  "remplace les éléments de n∞ <elem> par les valeurs <val>
extension: <fct> = fonction ; si <fct> différent de  ''='',
on remplace alors par (  <fct>   <val.ancienne>   <val>  )"
   (let* ((elem (list! elem))  (lg (1- (length elem)))
          (val (if (and (consp val) (one-elem elem)) (list val) (list! val))))
     (print elem)
     (print (posn-match elem '(1) ))
     (for (n 0 1 lg)
       (setq liste (substit-one liste (l-nth elem n) (l-nth val n) fct)))
     liste))
 

;-------------------------- filtres  ------------------------------

#|

; Same as in kernel but modified keep it???

(om::defmethod! band-filter ((list list) (bounds list) (mode symbol))
  :initvals '('(1 2 3 4 5) '((0 2) (5 10)) 'pass)
  :indoc '("list" "bounds" "mode" )
  :menuins '((2 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 235 
  :doc  "filters out <list> (a list or a tree of numbers) using <bounds>.
<bounds> is a list of pairs (min-value max-value). Elts in list are selected if they stay between the bounds.
<mode> is a menu input. 'Reject' means reject elts that are selected. 
'Pass' means retain only elts that are selected."
  (let ((bounds (if (atom (first bounds)) (list bounds) bounds)))
  (list-filter 
   #'(lambda (item)
       (some #'(lambda (bound) (and (>= item (first bound)) (<= item (second bound)))) bounds))
   list 
   mode)))
|#

;   band-pass band-reject   sont définis dans fichier  pw-list-functions

#|
(defunp b-pass ((list list) (min fix/float) (max fix/float)) list
 "garde les valeurs comprises entre min et max, bornes comprises"
  (filtre-liste '< min (filtre-liste '> max list)))

(defunp b-reject ((list list) (min fix/float) (max fix/float)) list
 "rejette les valeurs comprises entre min et max, bornes comprises"
  (append (filtre-liste '>= min list) (filtre-liste '<= max list)))
|#





; ajout de filtre-liste (= list-filter de PW)  - plus facile d'emploi que OM:list-filter

(defun filtre-liste1 (liste fct  val )  
"retire de la liste <list> toutes les valeurs répondant à la condition <fct val>"
  (let ((res))
    (dolist (num liste (nreverse res))
      (unless (funcall fct  num val) (push num res)))))

(om::defmethod! filtre-liste ((test symbol) (val number) (list list))
  :initvals '( '= 5 '(1 2 3 4 5 6 7 8 9))
  :indoc '("Test" "Valeur" "Liste")
  :icon 136
  :doc "filtre-liste  removes elements from a <list> according to a predicate <test>. If the 
predicate is 'eq', all instances of <val>  are removed from the list, regardless of 
their level. If, for example, the predicate is >, all elements of list which are 
greater than <val> are removed. Note that <val> can be a string, but only if the 
predicate <test> can handle a string. "
   (less-deep-mapcar 'filtre-liste1 list test val ))







(om::defmethod! multi-filter ((test symbol) (val number) (list list) (numcol integer))
  :initvals '( '= 7 '((1 2 3 4 5) (1 6 7 8 9)) 1)
  :indoc '("Test" "Value" "List of lists" "Num of column starting at 0")
  :icon 136
  :doc  
"
Correspond à l'ancien table-filter de PW
retire de chacune des sous-listes de <list> les éléments dont le numéro d'ordre
correspond à chaque élément de la sous-liste de numéro <numcol> qui satisfait
à la condition <test val>"
  (multi-fil test val list numcol))




(om::defmethod! band-multi-filter ((list list)  (min number) (max number) (numcol integer) (test symbol) )
  :initvals '('((1 3 15)  (a b c))  0.5 8 0 '<> )
  :indoc '("list" "Value" "min" "max" "numcol" "test")
  :icon 136
  :menuins '((4 (("<>"  '<> ) ("<>=" '<>=)  ("><"  '>< ) ("><=" '><= ))))
  :doc  "ne garde de chacune des sous-listes de <list> que les éléments dont le numéro d'ordre
correspond à chaque élément de la sous-liste de numéro <numcol> qui satisfait
à la condition <test min max>"

  (let ((fct (cond ((equal test '<>) 'compris )
                   ((equal test '<>=) 'compris= )
                   ((equal test '><) 'exclus )
                   ((equal test '><=) 'exclus= )))
        
        (longueur (length (nth numcol list))) 
        (ncol (1- (length list)))  res)

        (dotimes (n longueur)
          (if (funcall fct  (car (nth numcol list)) min max)
            
            (for (i 0 1 ncol)
             (push (car (nth i list)) res))  
            () )
          (setq list (ll-suppress list 0)) )

        (list-part (reverse res) (1+ ncol))) )




(om::defmethod! filtrenx ((liste list) (nbval integer) (fct  symbol))
   :initvals '( '(1 3 7 2 15)   3   '> )
   :indoc '( "liste" "nbval" "fct")
   :icon 136
   :doc   "laisse les <nbval> + grands ( > )  ou plus petits ( < ) d'une liste"
   (let ((fct (if (equal fct '>) 'list-max 'list-min)) (copie liste) val)
     (for (i 1 1 nbval)
       (setq val (funcall fct  liste))
       (setq liste (remove val liste)))
     
     (setq liste (x-diff copie liste))
     (setq nbval (length liste))
     (print (format nil "valeur limite = ~D  -  nb éléments retenus = ~D " val nbval))
     liste))


(om::defmethod! multi-filtrenx ((liste list) (nbval integer) (fct symbol) (numcol integer))
   
   :initvals '( '((1 3 7 2 15) (a b v f r))   3   '> 0)
   :indoc '( "liste" "nbval" "fct" "numcol")
   :icon 136
   :doc  "laisse les <nbval> + grands ( > )  ou plus petits ( < ) éléments
de la sous-liste <numcol>, et les éléments de numéro correspondant
des autres sous-listes
Il y des problËmes lorsque la liste comporte des valeurs égales proches de
la valeur limite - consulter les messages du listener"
   (let ( (fct (if (equal fct '>) 'list-max 'list-min))  
          (fct2 (if (equal fct '>) '< '>))
          (testcol (nth numcol liste)) val res)
     
     (if (>= nbval (length (first liste))) (setq res liste)
         (progn
           (for (k 1 1 nbval)
             (setq val (funcall fct  testcol))
             (setq testcol (remove val testcol)))
           (setq res (multi-fil fct2 val liste numcol))))
     
     (setq nbval (length (first res)))
     (print (format nil "valeur limite = ~D  -  nb éléments retenus = ~D ~%" val nbval ))
     res))
  
  

;-------------------------- traite-listes  ------------------------------


; densifier et monnayage sont également définis dans Esquisse (fichier Freq-Harmony)
; seule diff: le commentaire

(defun monnayage (list density)
  (let* ((interv (/ (- (second list) (car list)) (1+ density))) res)
    (for (n 1 1  density)
      (push (+ (car list) (*  n interv)) res))
    res))

(om::defmethod! densifier ((list list) (density integer) (nbdec integer) 
                              &optional (min nil)  (max nil))
   :initvals '('(1  5  9 20) 2  2 () () )
   :indoc '("List" "Density factor" "Nb decimals" "Low limit" "High limit")
   :icon 137
   :doc  "ajoute <density> valeurs entre chaque élément de la liste compris
entre min et max (optionnel). Les valeurs créées divisent les intervalles 
de la liste en <density> intervalles égaux. Attention: ce module n'ordonne
ni la liste d'entrée, ni la liste rendue
Extensions: min et max: l'opération n'aura lieu que pour tout 
intervalle dont les valeurs seront comprises entre min et max"
   (setq min (if min  min (list-min list))) 
   (setq max (if max  max (list-max list)))
   (let ((long (1- (length list))) res)
     (dotimes (n long)
       (push (car list) res)
       (if (and (>= (min (car list) (second list)) min)
                (<= (max (car list) (second list)) max)
                (> density 0))
         (push (monnayage list density) res))
       (setq list (cdr list)))
     (om-round (nreverse (flat (push (last-elem list) res))) nbdec ))) 



(om::defmethod! l-assoc ((format symbol)
                            (list1 list)  (list2 list) 
                            &rest lst?)

  :initvals '('flat  '(1 2) '(1 2) nil)
  :indoc '("format" "list1" "list2" "add list")
  :icon 137
  :menuins '((0 (("flat" 'flat) ("struct" 'struct) ("flat-low" 'flat-low))))
  :doc "couple les listes : (1 2 3) (10 11 12) --> (1 10 2 11 3 12)"


  (let* ((listgen (append (list list1 list2) lst?))
         (long (1- (l-max (mapcar #'length listgen)))) res)
    (for (i 0 1 long)
      (push  (car-mapcar #'l-nth listgen i) res))
    (cond  ((eq format 'flat) (flat (nreverse res))) 
           ((eq format 'struct) (nreverse res))
           ((eq format 'flat-low) (flat-low (nreverse res))))))


;===Rename l-assoc as l-associate 27-06-2007===================

(om::defmethod! l-associate ((format symbol)
                            (list1 list)  (list2 list) 
                            &rest lst?)

  :initvals '('flat  '(1 2) '(1 2) nil)
  :indoc '("format" "list1" "list2" "add list")
  :icon 137
  :menuins '((0 (("flat" 'flat) ("struct" 'struct) ("flat-low" 'flat-low))))
  :doc "Associates two lists. For example, (1 2 3) and (10 11 12) are associated into (1 10 2 11 3 12)."

  (l-assoc format list1 list2))

;=============================================================

(om::defmethod! create-matrix ((repeat integer) (liste list))
  :initvals '(3  '( 1 2 3 )) 
  :indoc '("number of repeats" "initial list")
  :icon 136
  :doc "Duplicates items in a list . If repeat = 3 and liste = '( 1 2 3 ),
returns  ((1 1 1) (2 2 2) (3 3 3)). Repeat can be a list of equal length than liste"
  (let ((res))
    (dolist  (l liste)
      (push (create-list repeat l) res))
    (nreverse res)))


(om::defmethod! create-matrix ((repeat list) (liste list))
 
    (loop for l  in liste
          for n in repeat
          collect (create-list n l)))




; gardé pour anciens patches

(defun l-couple (list1  list2)
"couple les listes : (1 2 3) (10 11 12) --> (1 10 2 11 3 12)"
  (flat (mapcar #'x-append list1 list2)))



(om::defmethod! inverseur ((liste list))
  :initvals '('(1 2) )
  :indoc '( "liste" )
  :icon 136
  :doc "renverse les valeurs, en gardant min et max" 
 
  (om-scale liste (list-max liste) (list-min liste)))



;====Rename inverseur as inverting 27-06-2007=========

(om::defmethod! inverting ((liste list))
  :initvals '('(1 2) )
  :indoc '( "liste" )
  :icon 136
  :doc "Returns the inverted list while maintaining minimum and maximum values." 
 
  (inverseur liste))

;=====================================================




(defun simplifie-rec (liste a-retirer nbmin vallim nbenleve fct)
  (let* ((nbelem (length liste))
        (elemvise (if (eq fct '<) (list-min liste) (list-max liste)))
        (position (position elemvise liste)))
    (cond 
     ((and (funcall fct elemvise vallim) (> nbelem nbmin) (< nbenleve a-retirer))
           (simplifie-rec (supprimelem liste position) a-retirer nbmin vallim (1+ nbenleve) fct))
     (t liste))))

(defun simplifie-1 (liste pcent% nbmin vallim fct)
  (let ((a-retirer (round (/ (* (length liste) pcent%) 100))))
    (simplifie-rec liste a-retirer nbmin vallim 0 fct)))


(om::defmethod! simpli-liste  ((liste  list )
                                  (pcent% number) (nbmin integer) 
                                  (vallim integer) 
                                  (fct symbol))



  :initvals '( '(1 2) 50 10 20 '<) 
  :indoc '("liste" "pcent%" "nbmin" "vallim" "fct")
  :icon 136
  :menuins '((0 ((">" '>)("<"  '<) )))
  :doc "retire d'une liste un %  d' éléments en partant des plus petits ou des
plus grands (selon la fct choisie)
On limite cependant la procédure avec nbmin = nb d'éléments qui doit rester
au minimum  et  vallim = valeur limite en-dessus ou en dessous de quoi
on agit"

  (less-deep-mapcar #'simplifie-1 liste pcent% nbmin vallim fct))


;===Rename simpli-liste as l-simplify 27-06-2007==================

(om::defmethod! l-simplify  ((liste  list ) (pcent% number) (nbmin integer) (vallim integer) (fct symbol))

  :initvals '( '(1 2) 50 10 20 '<) 
  :indoc '("list" "pcentage" "nbmin" "vallim" "function")
  :icon 136
  :menuins '((0 ((">" '>)("<"  '<) )))
  :doc "Removes from a list a certain percentage of the elements. Depending on the option chosen, either the smallest or the biggest values are removed from the list. With <nbmin> the minimum number of remaining elements can be chosen."

  (simpli-liste liste pcent% nbmin vallim fct))

;===================================================================




(om::defmethod! compl-list ((liste list) (long integer))
  :initvals '('((1  5  9 20) (4  3  2 1)) 1)
  :icon 136
  :indoc '("liste" "length")
  :doc   " compléte la liste pour obtenir la longueur voulue en répétant le dernier élément
Marche aussi si <liste> est un atome"


  (let* ((liste (list! liste))
        (lgliste (length liste)))
    (x-append liste (repeat-n  (last-elem liste) (- long lgliste)))))


;====Rename compl-list as l-complete 27-06-2007========================

(om::defmethod! l-complete ((liste list) (long integer))
  :initvals '('((1  5  9 20) (4  3  2 1)) 1)
  :icon 136
  :indoc '("liste" "length")
  :doc   "Returns a list completed to a desired length. This length is obtained by repetition of the last element. Also works for atoms at the input of <list>."

  (compl-list list long))

;=====================================================================



(defun posmax (llist col)
  (let ((nl (nth col llist)))
    (position (list-max nl) nl)))

(defun sort-table-rec (table col long res)
 (cond ((null (car table) ) res)
        (t  (for (i 0 1 long) 
              (push  (nth  (posmax table col) (nth i table))  (nth i res)))
            (sort-table-rec (ll-suppress table (posmax table col)) col long res))))
   
(om::defmethod! sort-table ((table list) (col integer)) 
  :initvals '('((1  5  9 20) (4  3  2 1)) 1)
  :icon 136
  :indoc '("table" "col")
  :doc   "trie en ordre croissant selon la colonne indiquée"
  (let* ((long (length table))
         (res (create-list long '())))
    (sort-table-rec table col (1- long) res)))




;============================ combinatoire  ============================



(om::defmethod! escalier ((liste list) (pas integer))
  :initvals '('(1 2 3 4 5 6) 2)
  :indoc '("List" "Step")
  :icon 136
  :doc   "permute en escalier : si pas = 2 ,(1 2 3 4 5 6)  devient (1 3 2 4 3 5 4 6)"
  (let ((res))
    (dotimes (i (- (length liste)  pas))
      (push (nth i liste) res)
      (push (nth (+ i pas) liste) res))
    (nreverse res)))

;===Rename escalier as stairs 26-06-2007===================

(om::defmethod! stairs ((liste list) (step integer))
  :initvals '('(1 2 3 4 5 6) 2)
  :indoc '("List" "Step")
  :icon 136
  :doc   "Returns a permutation according to a stairs model. For example, with a stepsize equal to 2, the list (1 2 3 4 5 6) will become (1 3 2 4 3 5 4 6)."

  (escalier liste step))

;==========================================================

(om::defmethod! scie ((liste list) (aller integer) (retour integer) (pas integer))
  :initvals '('(1 2 3 4 5 6 7) 4 2 1)
  :indoc '("List" "Aller" "Retour" "Step")
  :icon 136
  :doc  "permute en ''dents de scie'' : si aller = 4, retour = 2 , pas = 1:
(1 2 3 4 5 6 7)  devient (1 2 3 4 5 4 3 2 3 4 5 6 5 4 3 4 5 6 7 6 5)"

  (let ((res))
    (for (i 0 pas (- (length liste) 1 aller))
      (for (n i 1 (+ i aller))
        (push (nth  n liste) res))
      (for (n (1- (+ i aller)) -1 (- (+ i aller) retour))
        (push (nth  n liste) res)))
    (nreverse res)))

;===Rename scie as sawtooth 26-06-2007====================

(om::defmethod! sawtooth ((liste list) (go integer) (return integer) (step integer))
  :initvals '('(1 2 3 4 5 6 7) 4 2 1)
  :indoc '("List" "Aller" "Retour" "Step")
  :icon 136
  :doc  "Returns a permutation according to a sawtooth model. For example, when <go>=4, <return>=2 and <step>=1, the list (1 2 3 4 5 6 7) will become (1 2 3 4 5 4 3 2 3 4 5 6 5 4 3 4 5 6 7 6 5)."

  (scie liste go return step))


;==========================================================

(om::defmethod! aller-retour ((liste list) (modulo integer))
  :initvals '('(1 2 3 4 5 6)  2 )
  :indoc '("List" "modulo")
  :icon 136
  :doc  "(1 2 3 4 5 6) devient (1 3 5 6 4 2) avec modulo=2"
  (let* ((listmodulo (list-modulo liste modulo)) (res (first listmodulo)))
    (for (i 1 1 (1- modulo))
      (setf res (x-append res 
                          (if (= 0 (mod i 2)) (nth i listmodulo)
                              (reverse (nth i listmodulo))))))
    res))

;===Rename aller-retour as go-return 26-06-2007====================

(om::defmethod! go-return ((liste list) (modulo integer))
  :initvals '('(1 2 3 4 5 6)  2 )
  :indoc '("List" "modulo")
  :icon 136
  :doc  "Returns a permutation going forth and back through a list. When modulo is set to 2, the list (1 2 3 4 5 6) will become (1 3 5 6 4 2)."

  (aller-retour liste modulo))

;==================================================================


(defun  spire-rec (liste res)
  (push (car liste) res)
  (push (last-elem liste) res)
  (setq liste (cdr (butlast liste)))
  (if (null liste) res (spire-rec liste res)))


(om::defmethod! spirale ((liste list) (sens symbol ) )
  :initvals '('(1 2 3 4 5 6)  'cfug )
  :indoc '("List" "Direction")
  :menuins '((1 (("Centripetal" 'cpet) ("Centrifugal" 'cfug) )))
  :icon 136
  :doc   " (1 2 3 4 5 6) devient : centripËte -> (1 6 2 5 3 4) 
centrifuge -> 4 3 5 2 6 1"

  (let ((res () ))
    (if (equal sens 'cpet) (nreverse (spire-rec liste res)) (spire-rec liste res))))


;=================================================================


(om::defmethod! anaclase ((liste list) (pas integer))
  :initvals '('(1 2 3 4 5 6)  2 )
  :indoc '("List" "Step")
  :icon 136
  :doc  
"permutations de proximité  : si pas = 2 ,(1 2 3 4 5 6)  devient (1 3 2 5 4 6),
si pas = 3 ,(1 2 3 4 5 6 7)  devient (1 4 2 3 7 5 6) etc..."
  (let ((res (list (first liste))))
    (for  (i 1 pas (1-  (length liste)  ))
      (push (nth (+ i (1- pas)) liste) res)
      (for (n i 1 (+ i (- pas 2)))
        (push (nth n liste) res)))
      (nreverse (remove nil res))))

;===Rename anaclase as anaclasis 26-06-2007=====================

(om::defmethod! anaclasis ((liste list) (step integer))
  :initvals '('(1 2 3 4 5 6)  2 )
  :indoc '("List" "Step")
  :icon 136
  :doc "Permutations of proximity. For example, if the <step>=2 the list (1 2 3 4 5 6) will become (1 3 2 5 4 6). When <step>=3 the list (1 2 3 4 5 6) will become (1 4 2 3 7 5 6)."
  (anaclase list step))

;===============================================================

(om::defmethod! combinaisons ((liste list))
  :initvals '('(1 2 3 4 5 6)  )
  :indoc '("List" )
  :icon 136
  :doc  "combinaisons 2 à 2 des éléments d'une liste, sans répétition"
  (let ((ll (butlast liste)) (lm (cdr liste)) res)
    (while lm
      (dolist (l ll)
        (dolist (m lm)
          (push  (list l m) res))
        (pop lm)))
    (reverse res)))

;====Rename combinaisions as combining 26-06-2007============

(om::defmethod! combining ((liste list))
  :initvals '('(1 2 3 4 5 6)  )
  :indoc '("List" )
  :icon 136
  :doc  "Combines all elements of a list into pairs without repetitions."
  
  (combinaisons liste))

;==============================================================

(om::defmethod! unique% ((liste list) (pourcent number))
  :initvals '('(1 2)  0)
  :indoc '("List" "pourcent")
  :icon 136
  :doc  "retire certaines valeurs en double, selon un taux de hasard <pourcent>"

  (let ((res) )
    (dotimes (i (length liste))
      (if (not (and (< (random 100) pourcent) (member (nth i liste) res)))
        (push (nth i liste) res)))
    (nreverse res)))

;====Rename unique% as remove-dup% 26-06-2007==================

(om::defmethod! remove-dup% ((liste list) (percentage number))
  :initvals '('(1 2)  0)
  :indoc '("List" "percentage")
  :icon 136
  :doc  "Removes duplicates from a <List> according to a specified probability <percentage>."

  (unique% liste percentage))
      
;==============================================================

(defun sedim-1 (liste test val fonct)
  (let ((res (copy-list (groupe-tete  liste test val fonct))))
    (for ( i (1- (length res)) -1 1)
      (if (funcall test (nth i res) val)
        (progn (setf  (nth (1- i) res) (funcall fonct (nth (1- i) res) (nth i res)) )
               (setf  (nth i res) nil)) ) )
    (remove nil res)))

(defun attire-1 (liste test val fonct)
  (let ((res (copy-list (groupe-tete  liste test val fonct))))
    (do (( i 1 (1+ i))) ((null (nth i res)))
      (if (funcall test (nth i res) val)
        (progn (setf  (nth (1- i) res) (funcall fonct (nth (1- i) res) (nth i res)) )
               (setq res (l-suppress  res i ))
               (setq i (1- i)) )) )
    res))

(defun groupe-tete (liste test val fonct)
  (if (funcall test (car liste) val)
    (groupe-tete
      (setq liste (cons (funcall fonct (second liste) (car liste)) (cddr liste)))
        test val fonct)
     liste))

(om::defmethod! grouper ((test symbol) (val number) (liste list)
                            (fonct symbol) (format integer))
  :initvals '("<"  0 '(1 2) "+" 1)
  :indoc '("test" "val" "liste" "fonct" "format")
  :icon 136
  :doc  " regroupe valeurs: si ( <test> n+1  <val> ) est vrai  alors  on fait  n = ( <fonct> n  n+1 )
Ceci à l'intérieur de chaque sous-liste.
Format=1 : lit la liste à l'endroit: les petites valeurs sont ''aspirées'' 
par la grande valeur la plus à gauche
Format=0 : lit la liste à l'envers: les petites valeurs peuvent se regrouper jusqu'à former 
une valeur longue"

  (if (= format 1) (less-deep-mapcar 'attire-1 liste test val fonct)
                   (less-deep-mapcar 'sedim-1 liste test val fonct)))


;====Rename grouper as grouping 26-06-2007===========================

(om::defmethod! grouping ((test symbol)(val number)(liste list)(fonct symbol)(format integer))
  :initvals '("<"  0 '(1 2) "+" 1)
  :indoc '("test" "val" "liste" "fonct" "format")
  :icon 136
  :doc  "Regroups values inside a list following a predicate. When (<test> n+1 <val>) is true, then values are regrouped according to n = (<fonct> n  n+1) inside every sublist. 
In case <format>=1, the list is read forward: the small values are absorbed by the bigger value at their left.
In case <format>=0, the list is read backwards: the small values are regrouped (added to each other) to form one longer value."

  (grouper test val liste fonct format))

;=====================================================================

(defun perrec (liste)
  (if  (equal (car liste) (last-elem liste)) liste
    (append liste (list (l-nth (last-elem liste) (om- (last-elem liste) 1))))))

(om::defmethod! permut-rec ((liste list))
  :initvals '('(5 3 4 1 2))
  :indoc '("List")
  :icon 136
  :doc  "Recursive permutation (Messiaen): The list is read in the order given by its elements until the initial list is obtained (the list can only contain integers 1, 2, 3,...n)."

  ( perrec (list liste (l-nth liste (om- liste 1)))))


(om::defmethod! permut-circ ((list list)  (nth integer)) 
   :icon 136
  :doc  "Returns a circular permutation of <list> starting from its <nth> element (<nth>=0 means the 1st element of the list).

<nth> may be a list. If this is the case, the operation will lead to a list of permutations."

  (permut-circn/tm (copy-list list) nth))


(om::defmethod! permut-circ ((list list) (nth list)) 
  (loop for n in nth
        collect (permut-circ list n)))



(defun permut-circn/tm (list  nth )
  "Returns a destructive circular permutation of <list> starting from its <nth> (which
defaults to 1) element, (n=0 means the \"car\", n=1 means the \"cadr\")."
  (when list
    (let ((length (length list)) n-1thcdr)
      (setq nth (mod nth length))
      (if (zerop nth) list
          (prog1
            (cdr (nconc (setq n-1thcdr (nthcdr (1- nth) list)) list))
            (rplacd n-1thcdr ()))))))










(defun tm-proche (liste item)
  "recherche l'élément de <liste> le plus proche de <item>"

  (let ((listdif (om-abs (om- liste item))))
    (l-nth liste (position (list-min listdif) listdif) )))

(defun vocod1 (struct  reservoir  mode )
  
  (let* ((struct (if (equal mode 'freq) (mc->f  struct) struct))
         (reservoir (if (equal mode 'freq) (mc->f  reservoir) reservoir))
         (res))
    (dolist  (a struct)
      
      (push  (tm-proche reservoir a) res))
    (nreverse (if (equal mode 'freq) (f->mc  res) res))))

(om::defmethod! vocoder ((struct list) (reservoir list) 
                            &optional (mode 'midic))
   
   :initvals '('(1 2) '(1 2) 'midic)
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freqs))))
   :indoc '("struct" "reservoir" "mode")
   :icon 137
   :doc
   "Applies the structure of a list <struct> to a reservoir <reservoir>. It chooses those values from the reservoir which match the values in the structure most closely. <struct> may be a list of lists. <reservoir> is one single reservoir. For instance, <struct> may be a list of midicents, <reservoir> may be a another list of midicents which acts as a harmonic field, to which the <struct> is applied."
   
   (let ((struct (if (atom (car struct)) (list struct) struct)))
     (car-mapcar  'vocod1  struct reservoir mode)))








(defun max-abs-idt (ch1 ch2)
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (mapcar #'- ch1 ch2))
         (int-min (apply #'min ints))
         (int-max (apply #'max ints)))
    (values (/ (- int-max int-min) 2) (/ (+ int-max int-min) 2))))


(defun ma-min-interv (ch1 ch2)
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2>."
  (multiple-value-bind (dist ch) (max-abs-idt ch1 ch2)
    (declare (ignore ch))
    dist))



(om::defmethod!  vocod-transp1 ((struct list) (reservoir list) (pas number)
                                   &optional (mode 'midic))
   :initvals '('(1 2) '(1 2) 50 'midic)
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freqs))))
   :indoc '("struct" "resrvoir" "pas" "mode")
   :icon 137
   :doc  "comme ''vocoder'', mais cherche dans toute l'échelle de <reservoir>; i.e.
comme si on cherchait la transposition de <struct> s'appliquant le mieux"
   
   (let* ((debut (- (list-max struct) (list-min reservoir))) 
          (fin (- (list-min struct) (list-max reservoir)))
          (pas (if (< fin debut) (* pas -1) pas))
          listacc listdif)
     (for (transp debut pas fin)
       (let ((accres (vocoder (om- struct transp) reservoir mode)) )
         (push accres listacc)
         (push (ma-min-interv accres (om- struct transp)) listdif)))
     (l-nth listacc (position (list-min listdif) listdif) )))

(om::defmethod! vocod-transp ((struct list) (reservoir list) (step number)
                                   &optional (mode 'midic))

  :initvals '('(1 2) '(1 2) 50 'midic)
  :menuins '((3 (("Midics" 'midic) ("Freqs" 'freqs))))
  :indoc '("struct" "reservoir" "step" "mode")
  :icon 137
  :doc
  "Applies the structure of a chord <struct> or its transpositon to a harmonic field or reservoir <reservoir>. It chooses those values from the harmonic reservoir which match the structure of the chord or one of its transpositions most closely. <struct> may be a list of chords. <reservoir> can only be one harmonic field, i.e. one chord."


  (let ((struct (if (atom (car struct)) (list struct) struct)))
    (car-mapcar  'vocod-transp1  struct reservoir step mode)))
  

(om::defmethod! diff-sim ((fonda t) (nth list) 
                             (disto list) (orig  list)
                             (approx integer ) (compa symbol))
   
   :initvals '('(1 2) '(1 2) '(1 2) '(6000) 8 'diff)
   :menuins '((5 (("diff" 'diff) ("sim"'sim))))
   :indoc '("fonda" "nth" "disto" "orig" "approx" "compa")
   :icon 136
   :doc  "cherche un spectre fabriqué par 'n-sp-gen'  qui soit
le plus semblable ou le plus différent de l'accord donné
dans <orig>. Test effectué par 'common-notes'
arguments : 
fonda = fondamentale donnée ou liste de fondas
disto = distorsion donnée ou liste de distos
approx = approx choisie pour la comparaison
compa : choisir 'diff' ou 'sim'"
   
   (let* ((orig (chord->list! orig))
          (fonda (list! fonda)) (disto (list! disto)) (longd (length disto)) 
          notescom posi valm ftrouve dtrouve )
     (dolist (f fonda)
       (dolist (d disto)
         (push (length (common-notes (nth-polysp f nth d) approx orig)) notescom)))
     (setq notescom (nreverse notescom))
     (setq valm (if (equal compa 'sim) (list-max notescom ) (list-min notescom)))
     (format t "nb max ou min d'éléments communs: ~S ~%" valm)
     (setq posi (positions notescom valm))
     (setq  ftrouve (posn-match fonda (first (multiple-value-list (om// posi longd)))))
     (setq  dtrouve (posn-match disto (second (multiple-value-list (om// posi longd)))))
     (cond ((one-elem fonda) (format t "liste des distorsions: ~%") (om-round dtrouve 2))
           ((one-elem disto) (format t "liste des fondamentales: ~%") ftrouve)
           (t (format t "couples fondamentales/distorsions: ~%")
              (list-explode (l-couple ftrouve (om-round dtrouve 2)) (length ftrouve))))
     ))

;====Rename diff-sim as match-n-sp 25-06-2007===================

(om::defmethod! match-n-sp ((fonda t) (nth list) (disto list) (orig  list) (approx integer ) (compa symbol))
   
   :initvals '('(1 2) '(1 2) '(1 2) '(6000) 8 'diff)
   :menuins '((5 (("diff" 'diff) ("sim" 'sim))))
   :indoc '("fundamental" "nth" "distortion" "original" "approximation" "comparison")
   :icon 136
   :doc  "Looks for a spectrum made by ‘n-sp-gen’ that either resembles the chord <original> most closely or differs from it most. Effectively tests with ‘common-notes’.
Arguments:
<fundamental> = given fundamental or list of fundamentals
<distortion> = given distortion or list of distortions
<approximation> = the desired approximation (to 1/2 tone, 1/4 tone or 1/8th tone) before the common-note comparison is applied
<comparison> = mode of comparison, either resulting in the most similar chord through <sim> or the most different <diff>."

   (diff-sim fonda nth disto orig approx compa))

;==============================================================


(om::defmethod! diff-sim-scaling ((minout t) 
                                     (accord  t)        
                                     (maxout t) 
                                     (orig  t)
                                     (approx integer) 
                                     (compa symbol ))


  :initvals '('(1 2) '(6000) '(1 2) '(6000) 8 'diff)
  :menuins '((5 (("diff" 'diff) ("sim" 'sim))))
  :indoc '("minout" "accord" "maxout" "orig" "approx" "compa")
  :icon 136
  :doc  "cherche une distorsion d'un accord  qui soit
la plus semblable ou la plus différente de l'accord donné
dans <orig>. Test effectué par 'common-notes'
arguments : 
minout, maxout =  listes de valeurs à tester (l'un des 2 peut être un atom)
approx = approx choisie pour la comparaison
compa : choisir 'diff' ou 'sim'"


  (let* ((orig (chord->list! orig))
         (accord (chord->list! orig))
        (minout (list! minout)) (maxout (list! maxout)) (longd (length maxout)) 
        notescom posi valm mintrouve maxtrouve )
    (dolist (mn minout)
      (dolist (mx maxout)
        (push (length (common-notes (om-scale accord mn mx) approx orig)) notescom)))
    (setq notescom (nreverse notescom))
    (setq valm (if (equal compa 'sim) (list-max notescom ) (list-min notescom)))
    (format t "nb max ou min d'éléments communs: ~S ~%" valm)
    (setq posi (positions notescom valm))
    (setq  mintrouve (posn-match minout (first (multiple-value-list (om// posi longd)))))
    (setq  maxtrouve (posn-match maxout (second (multiple-value-list (om// posi longd)))))
    (cond ((one-elem maxout) (format t "liste des minout: ~%") mintrouve)
          ((one-elem minout) (format t "liste des maxout: ~%") maxtrouve)
          (t (format t "couples minout/maxout: ~%")
             (list-explode (l-couple mintrouve maxtrouve) (length mintrouve))))
      ))


;====Rename diff-sim-scaling as match-disto 25-06-2007=============

(om::defmethod! match-dist ((minout t) (accord  t) (maxout t) (orig  t) (approx integer) (compa symbol))
  :initvals '('(1 2) '(6000) '(1 2) '(6000) 8 'diff)
  :menuins '((5 (("diff" 'diff) ("sim" 'sim))))
  :indoc '("minout" "chord" "maxout" "original" "approximation" "comparison")
  :icon 136
  :doc  "Looks for the matching distortion of a chord (through scaling) that resembles the input chord <orig> most closely or differs from it most. Effectively tests with ‘common-notes’.
Arguments:
<minout, maxout> = lists of values defining the scaling processes that are tested (one of them may be an atom)
<approximation> = the desired approximation (to 1/2 tone, 1/4 tone or 1/8th tone) before the common-note comparison is applied
<comparison> = mode of comparison, either resulting in the most similar chord through <sim> or the most different <diff>."

  (diff-sim-scaling minout accord maxout orig approx compa))

;==============================================================

(om::defmethod! diff-sim-transpo ((transpo t) 
                                     (accord  t)        
                                     (approx integer) 
                                     (orig  t)
                                     (compa symbol))

  :initvals '('(1 2) '(6000)  8 '(6000) 'diff)
  :menuins '((4 (("diff" 'diff) ("sim" 'sim))))
  :indoc '("transpo" "accord"  "approx" "orig" "compa")
  :icon 136
  :doc  "Looks for a transposition of one chord <chord> that resembles another chord <original> most closely or differs from it most. Effectively tests with ‘common-notes’.
Arguments:
<transposition> = transposition value that is tested (atom or list)
<approximation> = the desired approximation (to 1/2 tone, 1/4 tone or 1/8th tone) before the common-note comparison is applied
<comparison> = mode of comparison, either resulting in the most similar chord through <sim> or the most different <diff>."
 

  (let* ((orig (chord->list! orig)) (accord (chord->list! accord))
        (transpo (list! transpo))   
        notescom posi valm  ttrouve )

    (dolist (mn transpo)
        (push (length (common-notes (om+ accord mn ) approx orig)) notescom))

    (setq notescom (nreverse notescom))
    (print notescom)
    (setq valm (if (equal compa 'sim) (list-max notescom ) (list-min notescom)))
    (format t "nb max ou min d'éléments communs: ~S ~%" valm)
    (setq posi (positions notescom valm))
    (setq  ttrouve (posn-match transpo  posi ))
    
    (format t "liste des transpo: ~%") ttrouve))


;====Rename diff-sim-transpo as match-trans 25-06-2007==========

(om::defmethod! match-trans ((transpo t) (accord  t) (approx integer) (orig  t) (compa symbol))
  :initvals '('(1 2) '(6000)  8 '(6000) 'diff)
  :menuins '((4 (("diff" 'diff) ("sim" 'sim))))
  :indoc '("transposition" "chord"  "approximation" "original" "comparison")
  :icon 136
  :doc  "Looks for a transposition of one chord <chord> that resembles another chord <original> most closely or differs from it most. Effectively tests with ‘common-notes’.
Arguments:
<transposition> = transposition value that is tested (atom or list)
<approximation> = the desired approximation (to 1/2 tone, 1/4 tone or 1/8th tone) before the common-note comparison is applied
<comparison> = mode of comparison, either resulting in the most similar chord through <sim> or the most different <diff>."

  (diff-sim-transpo transpo accord approx orig compa))

;=================================================================



; =================  Séries numériques ======================



(om::defmethod! arithm-crible ((begin integer) (end integer) 
                                  (numer integer) (denom integer))
   
   
   :initvals '(1 1  1 2 )
   :indoc '("begin" "end"  "numer" "denom")
   :icon 136
   :doc  "série arithmétique (entiers) avec crible numer/denom"
   
   (let ((cdeb (mod begin denom)) (pas (if (< end begin) -1 1)) res)
     (for (n begin pas end) 
       (if (not (>= (mod (- n cdeb) denom) numer)) (push n res)) )
     (nreverse res)))





(om::defmethod! n-arithm ((deb number) (fin number) (nbval integer)
                             &optional (format  'inclus))
   
   :initvals '(0 10 5 'inclus)
   :indoc '("deb" "fin" "nbval" "bornes inclues ou exclues")
   :menuins '((3 (("inclus" 'inclus) ("exclus" 'exclus))))
   :icon 137
   :doc  "série arithmétique : <nbval> éléments depuis deb jusqu'a fin"
   
   (let ((step (om/ (om- fin deb)  (1- nbval ))))
     (cond ((= nbval 1) fin)
           ((= deb fin) (create-list nbval deb))
           ((eq format 'exclus) (butlast (cdr (arithm-ser deb fin step ))))
           (t (arithm-ser deb fin step )))))
      


(om::defmethod! x-arithm ((deb number) (step number) (nbval integer)  )
   
   :initvals '(0 .1 5 )
   :indoc '("deb" "pas" "nbval")
   :menuins '((3 (("inclus" 'inclus) ("exclus" 'exclus))))
   :icon 136
   :doc  "série arithmétique : <nbval> éléments en cumulant <step> depuis deb "
   (dx->x deb (create-list (1- nbval) step)))

(om::defmethod! x-arithm ((deb number) (step list) (nbval list)  )
  (loop for s in step
        for n in nbval
        collect (x-arithm deb s n)))

(om::defmethod! x-arithm ((deb list) (step list) (nbval list)  )
  (loop for d in deb
        for s in step
        for n in nbval
        collect (x-arithm d s n)))


(defun fib-recur (liste end)
  (if (<= (last-elem  liste) end)
    (fib-recur (x-append liste (+ (last-elem  liste) (last-elem (butlast liste)))) end)
    (butlast liste)))

(defun fib-nb (liste nbval)
  (if (<= (length  liste) nbval)
    (fib-nb (x-append liste (+ (last-elem  liste) (last-elem (butlast liste)))) nbval)
    (butlast liste)))

(om::defmethod! fibonacci ((seed1 number) (seed2 number) 
                              (end number) &optional (nbval 1))
   :initvals '(1 2 10 1 )
   :indoc '("seed1" "seed2" "end" "nbval")
   :icon 137
   :doc  "série de Fibonacci, calculée à partir de 2 valeurs initiales, jusqu'à atteindre
<end>
Argument optionnel: nb de valeurs à calculer;si nbval>1, prend le pas sur <end>"
   
   (if (> nbval 1) (fib-nb (list seed1 seed2) nbval) (fib-recur (list seed1 seed2) end)))
        


(defun geom-recur (liste end facteur)
  (if (<= (last-elem  liste) end)
    (geom-recur (x-append liste (* (last-elem  liste) facteur)) end facteur)
    (butlast liste)))

(defun geom-nb (liste facteur nbval)
  (if (<= (length liste) nbval)
    (geom-nb (x-append liste (* (last-elem  liste) facteur)) facteur nbval)
    (butlast liste)))

(om::defmethod! geom-ser ((begin number) (end number) 
                             (facteur number) &optional (nbval 1))
   :initvals '(1 10 2 1 )
   :indoc '("begin" "end" "facteur" "nbval")
   :icon 137
   :doc  "série géométrique, calculée à partir de valeur initiale, et <facteur> ,
jusqu'à atteindre <end>
Argument optionnel: nb de valeurs à calculer;si nbval>1, prend le pas sur <end>"
(if (> nbval 1) (geom-nb (list begin) facteur nbval) (geom-recur (list begin) end facteur)))



(om::defmethod!  power-ser ((x0 number) (y0 number) (x1 number) (y1 number) (x2 number) (y2 number)
                  (nbval integer))
   :initvals '(0.1 0.1 0.1 0.1 0.1 0.1 1 )
   :indoc '("x0" "y0" "x1" "y1" "x2" "y2" "nbval")
   :icon 136
   :doc  "série puissance  on donne trois points : début, intermédiaire , fin
La fonction rend <nbval> valeurs entre début et fin selon fct puissance"
(power/3 (n-arithm x0 x2 nbval ) x0 y0 x1 y1 x2 y2 ))

  



(om::defmethod! puiss/to9-ser ((x0 number) (y0 number) 
                               (x1 number) (y1 number)
                               (x2 number) (y2 number)
                               (nbval number))
   :initvals '(1 1 2 3 4 5 6)
   :indoc '( "x0" "y0" "x1" "y1" "x2" "y2" "nbval")
   :icon 136
   :doc  "série puissance  on donne trois points : début, intermédiaire , fin
La fonction rend <nbval> valeurs entre début et fin selon fct puissance
Algorithme TO9 (marche mieux pour fct décroissante)"
  
(puiss/to9 (n-arithm  x0 x2  nbval)  x0 y0 x1 y1 x2 y2 ))




; Had to change the name because name of slot in OM
(om::defmethod! triangle-ser ((x0 integer ) (y0 integer )
                             (x1 integer ) (y1 integer )
                             (x2 integer ) (y2 integer))
   :initvals '(0 0 50 127 100 0 )
   :indoc '("x0" "y0" "x1" "y1" "x2" "y2")
   :icon 136
   :doc  "série numérique consistant en deux segments de droite"

  (let ((l1 (n-arithm y0 y1 (1+ (- x1 x0) )))
        (l2 (n-arithm y1 y2 (1+ (- x2 x1) ))))
    (if (one-elem l2) l1  (x-append l1 (rest l2))))) 
     


; fct présente dans PW (Num-series)

(om::defmethod! sinus-ser ((phase number) (nb-osc number)
               (nb-samples number) (amp t))
   :initvals '(0 1 8 1 )
   :indoc '("phase" "nb-osc" "nb-samples" "amp")
   :icon 136
   :doc  "parameters: phase = where we start on the sine curve (xmin)  
nb-osc = number of oscillations needed (-> determines xmax)
nb-samples = how many steps on the fragment of curve thus defined  
amplitude (ambitus normal -1 / 1)"


(let* ((xmin (* phase (/ pi 180))) (xmax (+ xmin (* 2 pi nb-osc)))
       (step (/ (- xmax xmin) (1- nb-samples))))
  (om*  amp (samplefun 'sin step xmin xmax)))) 





; =================== fonctions ======================================


 (om::defmethod! sample-fun ((fun function) (xmin number) 
                     (xmax number) (step number)) 
  :initvals '('+ 1 10 1 )
  :icon 136
  :doc   "Returns the list of values of <fun> from <xmin> to <xmax> with <step>.
For example:
(pw::sample-fun  'sin  0 1 6.3)
will return
? PW->(0.0 0.8414709848078965    0.9092974268256817     0.1411200080598672 
-0.7568024953079282     -0.9589242746631385     -0.27941549819892586)
and
(pw::sample-fun  (pw::make-num-fun '(f(x)= x + 1))  0 1 10)
will return
? OM->(1 2 3 4 5 6 7 8 9 10 11)
"
  (mapcar fun (arithm-ser xmin xmax step )))



(om::defmethod! bpf-transfer ((bpf bpf) (xval number ) &optional (nbdec nil))
  :initvals (list (make-instance 'bpf)  10 nil)
  :indoc '("BPF" "x" )
  :icon 136
  :doc "rend la valeur y correspondant à un x donné. Accepte liste ou liste de listes de xs "


(let* ((x1 (list-max (filtre-liste '> xval (x-points bpf))))
      (x2 (list-min (filtre-liste '< xval (x-points bpf))))

      (res (if (= x1 x2) (l-nth (y-points bpf) (position x1 (x-points bpf)))
               (funcall (linear-fun x1 (l-nth (y-points bpf) (position x1 (x-points bpf))) 
                                    x2 (l-nth (y-points bpf) (position x2 (x-points bpf))))
                        xval ))))
  (if (null nbdec) (float res) (om-round res nbdec))))



(defun transfer/map (xval bpf nbdec)   ; seulement pour que la liste xval soit le 1er argument pour le deep-mapcar
  (bpf-transfer bpf xval nbdec))

(om::defmethod! bpf-transfer ((bpf bpf) (xval list ) &optional (nbdec nil))
  (deep-mapcar/1 'transfer/map xval bpf nbdec)) 





; (linear parabole/2 parabole/3 power/2 power/3  )  
; sont définis dans PW : Num-fun-Gen
; puiss/TO9   est définie dans Esquisse : Freq-Harmony

;linear included in OM in Functions.lisp


;a utiliser comme lambda function
; par rapport à l'originale:
; le premier argument est nouveau c'est l'x propose  (sauf pour Lagrange)
; le dernier argument de l'ancienne fonction n'existe plus




(om::defmethod! tm-oper ((fun symbol) (obj1? list ) &optional (obj2? nil ))
  :initvals '('+ '(1 2) nil)
  :indoc '("Function" "Obj1" "Obj2" )
  :icon 137 
  :doc "Applies fun to leaves of trees of obj1? and (optionally) obj2?
(tm-oper   '+  4   5) will 
return  ? OM->9  ,
(tm-oper 'list  4 5) will return    ? OM->(4 5)   ,
(tm-oper '+  '(1 2) '( 3 4))  will return   ? OM->(4 6)  "

  (if obj2?
    (arith-tree-mapcar  (if (functionp  fun) fun (fdefinition fun)) obj1? obj2?)
    (deep-mapcar/1   fun obj1?)))


;====Rename tm-oper as tree-oper 27-06-2007===============================

(om::defmethod! tree-oper ((fun symbol) (obj1? list ) &optional (obj2? nil ))
  :initvals '('+ '(1 2) nil)
  :indoc '("Function" "Obj1" "Obj2" )
  :icon 137 
  :doc "Applies a given function to the leaves of trees of <obj1?> and (optionally) of <obj2>.

Examples:
(tree-oper '+  4 5) will return ? OM->9
(tree-oper 'list  4 5) will return ? OM->(4 5)
(tree-oper '+ '(1 2) '( 3 4))  will return ? OM->(4 6)"

  (tm-oper fun obj1? obj2?))

;=========================================================================



(om::defmethod! parabole ((x0 number) (y0 number) (x1 number) (y1 number)
                             &optional (x2 ()) (y2  9.0))

  :initvals '(1 1 2 4  () 9.0)
  :indoc '("X0" "Y0" "X1" "Y1" "X2" "Y2")
  :icon 137
  :doc  "Calculates the parameters of the equation  y = ax^2 + b  or y = ax^2 + bx + c 
as a function of the points (x0,y0)  (x1,y1) and (optional) (x2,y2) 
and creates the corresponding function  "
 (if x2 
   (parabole/3 x0 y0 x1 y1 x2 y2 )   
   (parabole/2 x0 y0 x1 y1 )))


(om::defmethod! parabole/2 ((x number) (x0 number) (y0 number)
                               (x1  number) (y1 number))
   :initvals '(1 1 1 2 4 )
   :indoc '("X" "X0" "Y0" "X1" "Y1" )
   :icon 136
   :doc "calcule les paramétres de l'équation  y = ax^2 + b  en fct de deux points 
(x0,y0) (x1,y1)
x = valeur(s) à calculer"
   
   (let* ((a   (/ (- y1  y0)  (- (* x1  x1)  (* x0  x0))  ))
          (b   (- y0   (* a  x0  x0)) )  )
     ; (format t "y = ~S x 2 + ~S ~%"  (om-round a 6) (om-round b 6)) 
     (+  (* a x x ) b )))

(om::defmethod! parabole/2 ((x list) (x0 number) (y0 number)
                               (x1  number) (y1 number))
 
   (let* ((a   (/ (- y1  y0)  (- (* x1  x1)  (* x0  x0))  ))
          (b   (- y0   (* a  x0  x0)) ) res  )
     (format t "y = ~S x 2 + ~S ~%"  (om-round a 6) (om-round b 6)) 
     (dolist (xn x)
       (push (+  (* a xn xn ) b ) res))
     (nreverse res)))

;====Rename parabole/2 as parabole2 in order to avoid / in the name 27-06-2007==========

(om::defmethod! parabole2 ((x number) (x0 number) (y0 number) (x1  number) (y1 number))
   :initvals '(1 1 1 2 4 )
   :indoc '("X" "X0" "Y0" "X1" "Y1" )
   :icon 136
   :doc "Calculates the parameters of the equation  y = ax^2 + b  as a function of the points (x0,y0)  (x1,y1) and (optional) (x2,y2) and creates the corresponding function."

   (let* ((a   (/ (- y1  y0)  (- (* x1  x1)  (* x0  x0))  ))
          (b   (- y0   (* a  x0  x0)) )  )
     ; (format t "y = ~S x 2 + ~S ~%"  (om-round a 6) (om-round b 6)) 
     (+  (* a x x ) b )))

(om::defmethod! parabole2 ((x list) (x0 number) (y0 number) (x1  number) (y1 number))

  (let* ((a   (/ (- y1  y0)  (- (* x1  x1)  (* x0  x0))  ))
          (b   (- y0   (* a  x0  x0)) ) res  )
     (format t "y = ~S x 2 + ~S ~%"  (om-round a 6) (om-round b 6)) 
     (dolist (xn x)
       (push (+  (* a xn xn ) b ) res))
     (nreverse res)))

;==========================================================================



(om::defmethod! parabole/3 ((x number) (x0 number) (y0 number)
                               (x1  number) (y1 number)
                               (x2 number) (y2 number)) 
   :initvals '(1 1 1 2 4 5 6)
   :indoc '("X" "X0" "Y0" "X1" "Y1" "X2" "Y2")
   :icon 136
   :doc  "calcule les paramétres de l'équation  y = ax^2 + bx + c en fct de trois points 
(x0,y0) (x1,y1) (x2,y2)
x = valeur(s) à calculer ."

   (let* ((a (/ (+ (* y0 (- x1 x2))
                   (* y1 (- x2 x0)) 
                   (* y2 (- x0 x1)))
                (+ (* x0 x0 (- x1 x2))
                   (* x1 x1 (- x2 x0))
                   (* x2 x2 (- x0 x1)))))
          (b (/ (+ y1 (- y2) (* a (- (* x2 x2) (* x1 x1))))
                (- x1 x2)))
          (c (- y2 (* a x2 x2) (* b x2))))
     ; (format t "y = ~S x 2 + ~S x + ~S  ~%" (om-round a 6) (om-round b 6) (om-round c 6))
     (+ (+  (* a x x )  (* b x) c))))


(om::defmethod! parabole/3 ((x list) (x0 number) (y0 number)
                            (x1  number) (y1 number)
                            (x2 number) (y2 number)) 
  
  (let* ((a (/ (+ (* y0 (- x1 x2))
                  (* y1 (- x2 x0)) 
                  (* y2 (- x0 x1)))
               (+ (* x0 x0 (- x1 x2))
                  (* x1 x1 (- x2 x0))
                  (* x2 x2 (- x0 x1)))))
         (b (/ (+ y1 (- y2) (* a (- (* x2 x2) (* x1 x1))))
               (- x1 x2)))
         (c (- y2 (* a x2 x2) (* b x2)))
         res)
    (format t "y = ~S x 2 + ~S x + ~S  ~%" (om-round a 6) (om-round b 6) (om-round c 6))
    
    (dolist (xn x)
      (push (+  (+ (+  (* a xn xn )  (* b xn) c)) b ) res))
    (nreverse res)))


;====Rename parabole/3 as parabole3 in order to avoid / in the name 27-06-2007=======================

(om::defmethod! parabole3 ((x number) (x0 number) (y0 number) (x1  number) (y1 number) (x2 number) (y2 number)) 
   :initvals '(1 1 1 2 4 5 6)
   :indoc '("X" "X0" "Y0" "X1" "Y1" "X2" "Y2")
   :icon 136
   :doc  "Calculates the parameters of the equation y = ax^2 + bx + c as a function of three points (x0,y0)  (x1,y1) and (x2,y2) and creates the corresponding function."

   (let* ((a (/ (+ (* y0 (- x1 x2))
                   (* y1 (- x2 x0)) 
                   (* y2 (- x0 x1)))
                (+ (* x0 x0 (- x1 x2))
                   (* x1 x1 (- x2 x0))
                   (* x2 x2 (- x0 x1)))))
          (b (/ (+ y1 (- y2) (* a (- (* x2 x2) (* x1 x1))))
                (- x1 x2)))
          (c (- y2 (* a x2 x2) (* b x2))))
     ; (format t "y = ~S x 2 + ~S x + ~S  ~%" (om-round a 6) (om-round b 6) (om-round c 6))
     (+ (+  (* a x x )  (* b x) c))))

(om::defmethod! parabole3 ((x list) (x0 number) (y0 number) (x1  number) (y1 number) (x2 number) (y2 number))

  (let* ((a (/ (+ (* y0 (- x1 x2))
                  (* y1 (- x2 x0)) 
                  (* y2 (- x0 x1)))
               (+ (* x0 x0 (- x1 x2))
                  (* x1 x1 (- x2 x0))
                  (* x2 x2 (- x0 x1)))))
         (b (/ (+ y1 (- y2) (* a (- (* x2 x2) (* x1 x1))))
               (- x1 x2)))
         (c (- y2 (* a x2 x2) (* b x2)))
         res)
    (format t "y = ~S x 2 + ~S x + ~S  ~%" (om-round a 6) (om-round b 6) (om-round c 6))
    
    (dolist (xn x)
      (push (+  (+ (+  (* a xn xn )  (* b xn) c)) b ) res))
    (nreverse res)))

;====================================================================
 

(om::defmethod! lagrange ((l-x-y list ))

  :initvals '('(1 10 3 25 10 100))
  :indoc '("List of points x y")
  :icon 136
  :doc "Returns a Lagrange polynomial defined by the points of list <l-x-y>. Connect 
to sample-fun to calculate values"

  (let ((length (length l-x-y)) index cp)
    (unless (evenp length)
      (error "You must give as many ys as xs in ~S." l-x-y))
    (unless (every #'numberp l-x-y)
      (error "l-x-y must contain only numbers ~S." l-x-y))
    (let* ((length (/ length 2))
           (vx (make-array length))
           (vy (make-array length))
           (Aitken (make-array length)))
      (setq length (1- length) index -1)
      (while l-x-y
        (incf index)
        (setf (aref vx index) (nextl l-x-y))
        (setf (aref Aitken index) (car l-x-y))
        (setf (aref vy index) (nextl l-x-y)))
      (for (j 1 1 length)
        (setq cp (aref Aitken (1- j)))
        (for (i j 1 length)
          (psetf (aref Aitken i) (/ (- (aref Aitken i) cp) (- (aref vx i) (aref vx (- i j))))
                 cp (aref Aitken i))))
      ;;(compile ()
      (eval `(function
              (lambda (x)
                (let* ((length ,length) (vx ',vx) (Aitken ',Aitken) (z (aref Aitken length)))
                  (for (i (1- length) -1 0)
                    (setq z (+ (aref Aitken i) (* z (- x (aref vx i))))))
                  z)))))))





(om::defmethod! linear-fct ((x number) (x0 number) (y0 number) (x1 number ) (y1 number ) )
  :initvals '(1 0 0 1 1)
  :indoc '( "x" "x0" "y0" "x1" "y1")
  :icon 136
  :doc 
  "Calculate the parameters of the equation y = a x + b as a function 
of the two points (x0,y0) (x1,y1). 
x = valeur(s) à calculer "
  (let* ((a (/ (- y1 y0) (- x1 x0)))
         (b (- y1 (* x1 a))))
    (format t "y = ~S x + ~S ~%" (om-round a 6) (om-round b 6) )
    
         (+ b (* x a))) )

(om::defmethod! linear-fct ((x list) (x0 number) (y0 number) (x1 number ) (y1 number ) )
  (let* ((a (/ (- y1 y0) (- x1 x0)))
         (b (- y1 (* x1 a)))
         res)
    (format t "y = ~S x + ~S ~%" (om-round a 6) (om-round b 6) )
    (dolist (xn x)
      (push (+ b (* x a)) res))                     
    (nreverse res)))

 
;--------------  puissances --------------------------


(om::defmethod! puiss/TO9 ((x number) (x0 number) 
                           (y0 number)(x1 number)
                           (y1 number)(x2 number)
                           (y2 number))
   :initvals '(1 1 2 3 4 5 6)
   :indoc '("x" "x0" "y0" "x1" "y1" "x2" "y2")
   :icon 136
   :doc   "calcule les paramétres de l'équation  y=a (x+c)^b+d en fct de trois points 
(x0,y0) (x1,y1) (x2,y2) .
La fct doit être continûment croissante ou décroissante.
Les points doivent être donnés dans l'ordre (donc : x2 > x1 > x0).
Utilise l'ancien algorithme du TO9.
Extrapolation à gauche interdite
x = valeur(s) à calculer"

  (let* ((c (- x0))
         (d y0)
         (b (/ (- (log (abs (- y2 y0))) (log (abs (- y1 y0)))) 
               (- (log (- x2 x0)) (log (- x1 x0)))))
         (a (if (>= y2 y0) (/ (- y1 y0) (expt (- x1 x0) b)) (/ (- y2 y0) (expt (- x2 x0) b)))))
(+ d (* a (expt (+ x c) b)))))


(om::defmethod! puiss/TO9 ((x list) (x0 number) (y0 number)(x1 number) (y1 number)
                           (x2 number) (y2 number))
(let* ((c (- x0))
         (d y0)
         (b (/ (- (log (abs (- y2 y0))) (log (abs (- y1 y0)))) 
               (- (log (- x2 x0)) (log (- x1 x0)))))
         (a (if (>= y2 y0) (/ (- y1 y0) (expt (- x1 x0) b)) (/ (- y2 y0) (expt (- x2 x0) b))))
         res)

  (dolist (xn x)
      (push (+ d (* a (expt (+ xn c) b))) res))
    (nreverse res)))


(om::defmethod!  power/2 ((x number) (x0 number)  
                             (y0 number) (x1 number)  
                             (y1 number))
  :initvals '(1 1 1 2 4)
  :indoc '( "x" "x0" "y0" "x1" "y1")
  :icon 136
  :doc  "calcule les paramËtres de l'équation  y=ax^b en fct de deux points (x0,y0) (x1,y1).
x = valeur(s) à calculer"
   
(if (zerop (* x1 x0 y0 y1))
  (progn (print "values of x and y must be different from zero") (beep))
  (let* ((b (/ (log (/ y1 y0)) (log (/ x1 x0))))
         (a (/ y1 (expt x1 b))))
    (format t "y = ~S x ** ~S ~%" (om-round a 10) (om-round b 10) )
     (* a (expt x b)))                      
    ))

(om::defmethod!  power/2 ((x list) (x0 number)  
                             (y0 number) (x1 number)  
                             (y1 number))
 (if (zerop (* x1 x0 y0 y1))
  (progn (print "values of x and y must be different from zero") (beep))
  (let* ((b (/ (log (/ y1 y0)) (log (/ x1 x0))))
         (a (/ y1 (expt x1 b)))
         res)
    (format t "y = ~S x ** ~S ~%" (om-round a 10) (om-round b 10) )
     (dolist (xn x)
       (push (* a (expt xn b)) res))                     
    (nreverse res))))



;;Taken from (c) Copyright Gerald Roylance 1982
(defun false-position-search (fcn s1 s2 eps)
  (do ((x1 s1)  (y1 (funcall fcn s1))
       (x2 s2)  (y2 (funcall fcn s2))
       (xn 0.0) (yn 0.0))
      (NIL)
    (declare (float x1 x2 y1 y2 xn yn))
    (if (= y1 y2) (return nil)) ;;(error "FALSE-POSITION-SEARCH Lost"))
    (setq xn (- x1 (* y1 (/ (- x2 x1) (- y2 y1)))))
    (setq yn (funcall fcn xn))
    (cond ((< (abs yn) eps) (return xn)))
    (cond ((> (abs y1) (abs y2))
	   (setq x1 xn) (setq y1 yn))
	  (t
	   (setq x2 xn) (setq y2 yn)))))



(defun power-search (x0 y0 x1 y1 x2 y2)
;bmin et bmax fixent les limites de la recherche du paramétre b par la 
;méthode dichotomique. Prendre un intervalle négatif pour une fct décroissante, 
;positif autrement. 19 ( ou -19) sont les valeurs maximales
  (setq x0 (float x0) x1 (float x1) x2 (float x2)
        y0 (float y0) y1 (float y1) y2 (float y2))
  (let* (a b c  res power
           (y/y (/ (- y2  y1)  (- y1  y0)))
           (bmin 0) (bmax 19)
           (growing (or (and (> x2 x1) (> y2 y1)) (and (< x2 x1) (< y2 y1))))
           (bmin-min (if growing 1.0 -19))  ;0.1))
           (incr (if growing 1 1))  ;0.1))
           (bmax (if growing 19 -1))) ;;1.0)))
    (setq b
          (do ((bmin bmin-min (+ bmin incr))) 
              ((>= bmin bmax) (and res (apply 'max res)))
            (setq power
                  (false-position-search  
                   #'(lambda (b) (- (* y/y (- (expt x1  b) (expt x0 b))) (- (expt x2  b) (expt x1  b))))
                   bmin bmax 0.001))
            (if power (push power res))))
    (unless b 
      (beep) 
      (error "sorry... couldn't find an interpolation with these values"))
    
    (setq a (/ (- y1 y0) (- (expt x1 b) (expt x0  b))))
    (setq c  (- y0 (* a (expt x0  b))))
    
    ;(format t "y = ~S x ** ~S + ~S  ~%" (om-round a 10) (om-round b 10) (om-round c 10))
    (list a b c)))




(om::defmethod! power/3 ((x number) (x0 number) (y0 number) 
                            (x1 number) (y1 number) (x2 number) (y2 number))
   :initvals '(1 1 1 2 4 3 9)
   :indoc '( "x" "x0" "y0" "x1" "y1" "x2" "y2" )
   :icon 136
   :doc   "calcule les paramétres de l'équation  y=ax^b+c en fct de trois points 
(x0,y0) (x1,y1) (x2,y2) et crée la fonction correspondante .
La fct doit être continûment croissante ou décroissante.
x = valeur(s) à calculer"
   
   (let* ((param (power-search x0 y0 x1 y1 x2 y2))
     (a (first param))
     (b (second param))
     (c (third param)))
        (+ c (* a (expt x b)))))

 

(om::defmethod! power/3 ((x list) (x0 number) (y0 number) 
                         (x1 number) (y1 number) (x2 number) (y2 number) )
(let* ((param (power-search x0 y0 x1 y1 x2 y2))
         (a (first param))
         (b (second param))
         (c (third param))
         solutions)
   (dolist ( nx x)
      (push (+ c (* a (expt nx b))) solutions))
    (nreverse solutions)))




#|
(om::defmethod! power-fct ((x list) (x0 number) (y0 number) (x1 number) (y1 number)
                   &optional (x2 ()) (y2  9.0))

  :initvals '(1 1 1 2 4  '() 9.0)
  :indoc '("X0" "Y0" "X1" "Y1" "X2" "Y2")
  :icon 137
  :doc  "Calculates the parameters of the equation y = a x b + c  or y = a x b   
as a function of the points (x0,y0)  (x1,y1) and (optional) (x2,y2) 
and creates the corresponding function  "
 (if x2 
   (power/3 x x0 y0 x1 y1 x2 y2)   
   (power/2 x x0 y0 x1 y1 )))

|#


 
(om::defmethod! bpf-gen ((pts list) (nbdec integer))
   
   :initvals '('((1 2) (3 4))  0)
   :indoc '("pts" "nbdec")
   :icon 136
   :doc   ""
   
   (let (res)
     (for (i 0 2 (- (length pts) 3))
       (push (second (nth i pts)) res)
       (push (interpolation (second (nth i pts)) (second (nth (+ 2 i) pts))
                            (- (first (nth (+ 2 i) pts)) (first (nth  i pts)))
                            (first (om-round (nth (1+ i) pts) nbdec))
                            ) res)   )
     (push (second (first (last pts))) res)
     (cdr(butlast (om-round (flat (nreverse res)) nbdec)))))




; ========================= traitements de fonctions  ===========================


(om::defmethod! thales ((newmin number) (newmax number) (liste list))
   
   :initvals '(0 0  '(1 2))
   :indoc '("newmin" "newmax" "liste" )
   :icon 136
   :doc   "régle de trois sur une liste ; 
on donne 2 points de référence: nouveau minimum, nouveau maximum 
(équivaut à  DISTOR )"
   
   
   (mapcar (linear (list-min liste) newmin (list-max liste) newmax) liste))



;    l-distor/2 l-distor/3 L*line L*curb/2 L*curb/3 
; sont définis dans  Esquisse : Freq-Harmony

; ces fct ne marchent pas !!!  linear, ec.. sont probablement differents dans OM


; il faudrait tester si la liste contient + qu'un élément
(om::defmethod!  L*line ((fact1st number) (factlast number) (liste list))
   :initvals '(0.1 1.2  '(1 2))
   :indoc '("fact1st" "factlast" "liste" )
   :icon 136
   :doc   "Multiplies a list, <liste>, by a linear function. The first element is 
multiplied by <fact1st>, the last by <factlast> and all intermediate 
elements by linear interpolations between those values."

  (let ((long (length liste)))
    (om* liste  (sample-fun (linear-fun 1 fact1st long factlast) 1  long 1 ))))

;................. ces 2 fct sont remplacées dans le menu par L*curb ............

(om::defmethod!  L*curb/2 ((fact1st number) 
                              (factlast number) (liste list))
   :initvals '(1.0 2.0 '(1 2))
   :indoc '("fact1st" "factlast" "liste")
   :icon 136
   :doc   "Multiplies a list, <liste>, by a power function. The first element is 
multiplied by <fact1st>, the last by <factlast> and all intermediate 
elements by interpolations along a power function between those values."
   
   
   (let ((long (length liste)))
    (om* liste (power/2 (arithm-ser 1 long 1) 1 fact1st long factlast)) ))


(om::defmethod! L*curb/3 ((fact1st number) (factlast number)
                             (ref integer) (factref number )
                             (liste list))

   :initvals '(1.0 4.0 2 3.0 '(1 2 3 4))
   :indoc '("fact1st" "factlast" "ref" "factref" "liste")
   :icon 136
   :doc  "Multiplies a list, <liste>, by a power function. This box is identical to 
'l*curb/2' except that a reference point is controllable. The first 
element is multiplied by <fact1st>, the last by <factlast> and the 
element of the original list with a value of <ref> will be multiplied by 
<factref>. All intermediate elements will be multiplied by interpolations 
along a power function between <fact1st> and <factlast>. The power 
function, however, will be altered to accommodate the reference point."


  (let ((long (length liste)))
    (if (or (> fact1st factref factlast) (< fact1st factref factlast))
      (om* liste (puiss/TO9 (arithm-ser 1 long 1) 1 fact1st ref factref long factlast))
      (om* liste (power/3 (arithm-ser 1 long 1) 1 fact1st ref factref long factlast)))))

 
(om::defmethod! L*curb ((liste list) (fact1st number) (factlast number)
                           &optional  (ref nil) (factref 2 ))
  :initvals '('(1 2 3 4) 1.0 4.0 nil 3.0 )
   :icon 137
   :doc  "Multiplies a list, <liste>, by a power function. The first 
element is multiplied by <fact1st>, the last by <factlast>; optionally the 
element of the original list with a value of <ref> will be multiplied by 
<factref>. All intermediate elements will be multiplied by interpolations 
along a power function between <fact1st> and <factlast>. The power 
function will be altered to accommodate the optional reference point."

(if (one-elem liste)
  (om* fact1st liste)
  (if (null ref) (L*curb/2 fact1st factlast liste ) (L*curb/3 fact1st factlast ref factref liste ))))

;....................................................................



(defun l-min (list)
  "minimum value(s) of a list or list of numbers"
  (if (not (consp (first list)))
    (apply 'min list)
    (mapcar #'(lambda (x) (apply 'min x)) list)))

(defun l-max (list)
  "maximum value(s) of a list or list of numbers"
  (if (not (consp (first list)))
    (apply 'max list)
    (mapcar #'(lambda (x) (apply 'max x)) list)))


(om::defmethod! l-distor/2 ((newmin number ) (newmax number)
                               (liste list))
:initvals '(1 2 '(0.5 0.7))
   :indoc '("newmin" "newmax" "liste")
   :icon 136
   :doc "Distorts a list, <liste>, by a power function, thus if the list is linear 
the result follow the power function, if the list is non-linear the 
result will be a hybrid of the old liste and the power function. 
The arguments <newmin> and <newmax> determine the scaling of the new 
list. (<newmin> will be the smallest value present, <newmax> the largest)"
(let ((liste (list! liste)))
    (mapcar #'(lambda (x) (power/2 x (l-min liste) newmin (l-max liste) newmax)) liste)))



(om::defmethod! l-distor/3 ((newmin number) (newmax number)
                               (ref number) (newref number)
                               (liste list))
   :initvals '(0.5 1.0 0.7 2 '(0.6 0.9))
   :indoc '("newmin" "newmax" "ref" "newref" "liste")
   :icon 136
   :doc "Distorts a list, <liste>, by a power function, thus if the list is linear 
the result will follow the power function, if the list is non-linear the 
result will be a hybrid of the old liste and the power function. This box
is identical to 'l-distor/2' except that a reference point is 
controllable.
The arguments <newmin> and <newmax> determine the scaling of the new 
list. (<newmin> will be the smallest value present, <newmax> the largest)
The values <ref> and <newref> are used to specify that the element of the 
original list with a value of <ref> will be moved to the value of 
<newref>. The curve will be altered in order to accommodate the reference 
point."
   (if (or (> newmin newref newmax) (< newmin newref newmax))
  (mapcar #'(lambda (x) (puiss/TO9 x (l-min liste) newmin  ref newref (l-max liste) newmax )) liste)
  (mapcar #'(lambda (x) (parabole/3 x (l-min liste) newmin  ref newref (l-max liste) newmax )) liste)))




(om::defmethod! deformer ((ref number ) (newref number ) (liste list ))
   :initvals '(1.0 4.0 2 3.0 '(1 2 3 4))
   :indoc '("ref" "newref" "liste")
   :icon 136
   :doc "Change la ''courbure'' d'une liste, en conservant
le min et le max; on donne un point de référence
intermédiaire: 
ancienne valeur -> nouvelle valeur"

  (l-distor/3 (list-min liste) (list-max liste) ref newref liste))


(defun deform-1 (liste pcent )
  (let ((maxim (list-max liste)))
  (l-distor/3 (list-min liste) maxim (/ maxim 2) (/ (* maxim pcent) 100) liste)))

(om::defmethod! deformer% ((liste list ) (pcent number))
   :initvals '('(1 2 3 4) 50)
   :indoc '("liste" "pcent")
   :icon 136
   :doc "Change la ''courbure'' d'une liste, en conservant
le min et le max; on donne un pourcentage d'effet (0-100)
qui agira sur la position de la valeur médiane considérée
comme point de référence ->  pcent=50 = pas d'effet"

  (less-deep-mapcar 'deform-1 liste pcent))



(defun ll-deform-1 (liste fct)
  (mapcar  fct liste )) 

(om::defmethod! ll-deformer% ((liste list )(pcent number))
   :initvals '('((1 2) (3 4)) 50)
   :indoc '("liste" "pcent")
   :icon 136
   :doc "comme deformer%, mais toutes les sous-listes sont soumises
à la même déformation (avec deformer%, la déformation est différente
pour chaque sous-liste, en fct des min et max propres)"

  (let* ((minim (list-min (list-min liste))) (maxim (list-max (list-max liste)))
        (fct (lambda (x) (puiss/TO9 x minim minim  (/ maxim 2) (/ (* maxim pcent) 100) maxim maxim ))))
    (less-deep-mapcar 'll-deform-1 liste fct)))




(defun smooth2 (list)
  (let (a b c newlist (list `(,(/ (+ (first list) (second list)) 2.0)  ,.list 
                              ,(/ (+ (first (last (butlast list))) (first (last list))) 2.0))))
    ;(print list)
    (while (cddr list)
      (setf a (pop list) b (first list) c (second list))
      (setf b (float (average2  a b c)))
      ;(setf (first list) b)
      (push b newlist))
    (nreverse newlist)
    ))

(defun average2 (&rest list)
  (/ (apply '+ list) (length list)))

(om::defmethod! lisser ((list list) (order integer))
   :initvals '('(1 2) 1)
   :indoc '("liste" "order")
   :icon 136
   :doc ""
""
  (dotimes (i order)
    (setf list (smooth2 list)))
  list)


;====Rename lisser as smoothing 27-06-2007============

(om::defmethod! smoothing ((list list) (order integer))
   :initvals '('(1 2) 1)
   :indoc '("liste" "order")
   :icon 136
   :doc "Smoothing of a list of values by averaging between the successive elements of the list. For example, the list (1 2 3 4 5 10 1 12 13 17) will become (1.5 2.0 3.0 4.0 6.33 5.33 7.67 8.67 14.0 15.0)."

   (lisser list order))


; ====================================================






; =======================   chaos  ====================



(defun fractrec (liste listdiv prof)
  (if (eq prof *n) (diffract liste listdiv)
    (fractrec (diffract  liste listdiv) listdiv (1+ prof))))

(om::defmethod! fractal+ ((liste list) (listdiv list) (level integer))

  :initvals '('(1 2) '(1 2) 0)
  :indoc '("liste" "listdiv" "level")
  :icon 136
  :doc "fractalisation de la liste <liste> selon le découpage <listdiv>
 avec <level> niveaux de récursion"
  (if (= level 0) 
    (setq liste liste)
    (let ((*n (1- level))) 
      (declare (special *n))
      (fractrec liste listdiv 0))))





;...............anciennes fonctions...........................................
(defun fract-recurs (liste prof)
 (if (eq prof *n)
  (list liste)
  (flat-once (mapcar #'(lambda (item) (fract-gen item liste prof)) liste))))


(defun fract-gen (num liste prof)
  (let ((sum (apply '+ liste)))
    (fract-recurs (mapcar #'(lambda (item) (/ (* num item) sum)) liste) (1+ prof))))


(defun fractal (liste level )
"fractalisation de la liste avec <level> niveaux de récursion"
  (let ((*n level)) (declare (special *n))
       (fract-recurs liste 0)))

(defun fract1 (liste listdiv)
  (let ((res ()) (long (length liste))) 
    (dotimes (n long)
      (push (om-scale/sum listdiv (car liste))res) 
       (setq liste (cdr liste)))
    (reverse res)))

(defun diffract (liste listdiv)
"fractalise une fois, selon la liste <listdiv>, la liste <liste> 
de niveau quelconque"
  (lldecimals (less-deep-mapcar 'fract1  liste listdiv) 4))


; ...............................................................


(defun logistic-rec (x taux iter i res)
  (push (* taux x (- 1 x)) res) 
  (if (< i iter)
     (logistic-rec (* taux x (- 1 x)) taux iter (1+ i) res)
     (nreverse res)))

(om::defmethod! logistic ((xdeb  number) 
                             (taux number)
                             (iter number))

  :initvals '( 0.02 2.7 30)
  :indoc '("xdeb" "taux" "iter")
  :icon 136
  :doc "équation 'différentielle logistique'
Simule une évolution de population en fonction de population
de départ (xdeb) et d'un taux . Iter = nombre de fois où 
l'on répéte l'équation :  x = rx (1 - x)
x doit être < 1"


    (logistic-rec xdeb taux iter 1 ()))



(defun henon-rec (x y taux iter i resx resy)
  (push (- (1+ y ) (* taux x x)) resx)
  (push (/  x 3) resy)
  (if (< i iter)
     (henon-rec (- (1+ y ) (* taux x x)) (/  x 3) taux iter (1+ i) resx resy)
     (list (nreverse resx) (nreverse resy))))
 
(om::defmethod! henon ((xdeb  number ) (ydeb number )
                          (taux number ) (iter number ))

  :initvals '( .9 .8 1.4 30)
  :indoc '("xdeb" "ydeb" "taux" "iter")
  :icon 136
  :doc " attracteur de Hénon
Simule une évolution de population en fonction de population
de départ (xdeb) et d'un taux . Iter = nombre de fois où 
l'on répéte l'équation :  x = rx (1 - x)
x doit être < 1 "
    (henon-rec xdeb ydeb taux iter 1 () ()))



; ================== alea ==================================



(om::defmethod! LLalea ((list list) (percent% number))
   :initvals '('(2 3 3 5 8) 0.1)
   :indoc '("list" "vrai pourcentage: écrire <10> pour 10%")
   :icon 136
   :doc  "Ajoute ou retranche aléa à la liste (de profondeur quelconque) 
selon % indiqué"
   (deep-mapcar/1  'mulalea list (/ percent% 100)))



(om::defmethod! tirage ((percent% number))
   :initvals '(50)
   :indoc '("vrai pourcentage: écrire <10> pour 10%")
   :icon 136
   :doc  "sort nil ou t selon % de chances indiqué: ie. 30 % = 30% de chances de sortir t"
(<(om-random 0 100)  percent% ))



(defun rec-tirage (nbelem larithm res n)
  (let ((tirage (nth-random larithm)))
    (push tirage res)
    (setq larithm (remove tirage larithm))
    (setq n (1+ n))
    (if ( = n nbelem )
      res
      (rec-tirage nbelem larithm res n))))


(om::defmethod! list-tirage ((liste list) (nbelem integer))
   :initvals '('(1 2 3 4 5) 3)
   :indoc '("liste simple" "nb elem à tirer")
   :icon 136
   :doc  "garde aléatoirement nbelem éléments de la liste"
   (let* ((larithm (arithm-ser  0  (1- (length liste)) 1)) )
     (posn-match liste (sort (rec-tirage nbelem larithm nil 0) '< ))))


(om::defmethod! list-alea-filter ((liste list) (percent% number))
   :initvals '('(1 2 3 4 5) 50)
   :indoc '("liste simple" "vrai pourcentage: écrire <10> pour 10%")
   :icon 136
   :doc  "garde aléatoirement percent% des éléments de la liste"
(let* ((larithm (arithm-ser  0  (1- (length liste)) 1))
      (nbelem (round (* (length liste) (/ percent% 100.)))))
  (if (< nbelem 1) nil
   (posn-match liste (sort (rec-tirage nbelem larithm nil 0) '< )))))



(om::defmethod! random-list ((nb integer) (low number) (high number))
   :initvals '(10 0 1)
   :icon 136
   :doc  "créée une liste  de nb éléments aux valeurs aléatoires entre low et high"
(let ((res))
(for (i 1 1 nb)
  (push (om-random low high) res))
res))


(om::defmethod! random-from-list ((liste list) (nb integer)  )
   :initvals '(3 '(1 2 3 4 5))
   :icon 136
   :doc  "tire au hasard nb éléments dans la liste ; pour ne pas avoir de répétitions
utiliser list-tirage "
(let ((res))
(for (i 1 1 nb)
  (push (nth-random liste) res))
res))

; =======================  arithmétique ====================





(om::defmethod! om-modulo ((numbers t) (mod number))
  :initvals '('(10 11 12) 3)
  :indoc '( "numbers" "mod")
  :icon 136
  :doc  "Calculates the number that is congruent modulo mod to numbers, or the remainder
of an integer division (Euclidean division between two numbers numbers and mod). "
  (arith-tree-mapcar (function mod) numbers mod))


(om::defmethod! om-floor ((numbers t) &optional (div 1))
  :initvals '( 52.71 1)
  :indoc '( "numbers" "div")
  :icon 137
  :doc    "Truncation of number or tree. Rounded to the larger integer. "
  (deep-mapcar/1 'floor numbers div))




(om::defmethod! cumul ((liste list))
  :initvals '('(1 2 3 4 5))
  :indoc '( "liste" )
  :icon 136
  :doc   "Cumuls successifs de la liste "
  (let ((x 0))
    (deep-mapcar/1 #'(lambda (dx) (incf x dx)) liste)))




(om::defmethod! diff ((liste list))
  :initvals '('(2 3 3 5 8))
  :indoc '( "liste" )
  :icon 136
  :doc  "Returns the list of the intervals between the contiguous values of a list,
   starting from 0."
  (cons (car liste) (mapcar #'- (cdr liste) liste)))



(defun comparres (modliste toler)
  (let ((res t) (azaz (mapcar '<= modliste toler)))
    (dolist (i azaz)
      (if (null i) (setq res nil)))
    res))

(om::defmethod! l-pgcd ((liste list) (precision number) (pas number))
   :initvals '('(45 81 103 127) 1 0.1)
   :indoc '( "liste" "precision" "pas" )
   :icon 136
   (let ((depart (list-min liste)) (toler (om* liste (/ precision 100)))   res)
     (do  (  (i depart (- i pas)) )
          ((< i pas))
       (setq res i)
       (if (comparres  (om-modulo liste i) toler) (return)))
     (om-round res 8)))
 




(om::defmethod! ll-scaling ((list list) (min number) (max number))
   :initvals '('((1 3 15) (6 7 8)) 0.5 20)
   :indoc '( "liste" "min" "max")
   :icon 136
   :doc  "scaling d'une liste de liste en fct du max et du min
globaux de toutes les sous-listes confondues"
   (om-scale list min max (list-min (list-min list)) (list-max (list-max list))))


(defun scale% (num %scfc)
  "Returns an integer that is scaled as %scfc percentage of num."
  (round (* num %scfc) 100))

(om::defmethod! l-scale% ((l1? list) (l2? list))
   :initvals '('(1 3 15) '(6 7 8))
   :indoc '( "list1" "list2")
   :icon 136
   :doc
   "Divides by 100 the product of <l1?> and <l2?>."
   (double-mapcar 'scale% l1? l2?))


(om::defmethod! scale-r ((liste list) (min number) (max number) (dec integer))
   :initvals '('(1 3 15 6 7 8) 0.5 20 2)
   :indoc '( "liste" "min" "max" "dec" )
   :icon 136
   :doc
   "om-scaling avec arrondi"
   (om-round (om-scale liste min max) dec))




(defun compris (val min max)
  (and (< val max) (> val  min)))

(defun compris= (val min max)
  (and (<= val max) (>= val  min)))

(defun exclus (val min max)
  (or (> val max) (< val  min)))

(defun exclus= (val min max)
  (or (>= val max) (<= val  min)))


   
(om::defmethod! <> ((l? t) (min number) (max number)
            (bornes? symbol))

   :initvals (list 5 1 10  '<>=)
   :indoc '("l?"  "min" "max" "bornes")
   :menuins '((3 (("<>"  '<>)
                 ("<>=" '<>= ))))
   :icon 136
   :doc "l? est-il dans l'intervalle min max ?"

  (let ((fct (if (equal bornes? '<>) 'compris 'compris=)))
    (car-mapcar  fct l? min max)))


(om::defmethod! >< ((l? t) (min number) (max number) (bornes? symbol ))
  :initvals '('(1 3 15)  0.5 3 '>< )
  :indoc '("l?"  "min" "max" "bornes")
  :icon 136
  :menuins '((3 (("><"  '><) ("><=" '><=) )))
  :doc "l? est-il hors de l'intervalle min max ? - bornes incluses ou excluses"
  (let ((fct (if (equal bornes? '><) 'exclus 'exclus=)))
    (car-mapcar  fct l? min max)))
   




(om::defmethod! x->dx+ ((liste list) (elem number))
   :initvals '('((1 3 15) (6 7 8)) 0.5 20)
   :indoc '("liste"  "elem")
   :icon 136
   :doc "comme x->dx,  mais ajoute un élément final"
   (x-append (x->dx liste) elem))



(om::defmethod! l-prime? ((liste list ))
  :initvals '('((1 3 15) (6 7 8)) 0.5 20)
  :indoc '("liste")
  :icon 136
  :doc "Teste  liste de nb premiers - doivent Ítre < 99460729."
  (mapcar 'prime? liste))



(defun  accumule-rec (accum fct liste res)
  (setq liste (cdr liste))
  (setq accum (funcall fct accum (first liste)))
  (push  accum res)
  (if (one-elem liste) res (accumule-rec accum fct liste res)))




(om::defmethod! accumule ((fct t ) (liste list) 
                          (format symbol ))
  
  :initvals '('+ '(1 2) 'incl)
  :indoc '("fct"  "liste" "format")
  :menuins '((2 (("incl" 'incl) ("excl" 'excl) )))
  :icon 136
  :doc "effectue une opération cumulative sur une liste. La fonction fct est
appliquée sur les 2 1ers éléments, puis sur le résultat obtenu et le
3e élément, etc... La premiére valeur peut être incluse ou excluse.
Ex : si la fct est +   (1 2 3 5) rend (1 3 6 11)"
  
  (let* ((accum (first liste)) (res (if (equal format 'excl) ()  (list (first liste)))))
    (nreverse (accumule-rec accum fct liste res))))

; ============================== midi ==========================

; ajouter port

(om::defmethod! joue ((chord chord ) (approx integer ) &optional (port 0))
   :initvals '( nil 4 0)  
   :icon 137
(midi-o (mat-trans (list (om+ 143 (om+ (lchan chord)  (om-round (om/ 
                               (om-modulo (approx-m (lmidic chord) approx) 100) 25))))
                           (om-floor (approx-m (lmidic chord) approx) 100)
                           (lvel chord) )) port) )
  
 


(om::defmethod! tx-duo-pgm ((son1 integer ) (son2 integer ) &optional (port 0))
   :initvals '( 1 1 0)  
   :indoc '("son1"  "son2")            
   :doc "numéros de prg--> TX1/4, TX5/8"
   :icon 137
   (pgmout son1 '(1 2 3 4) port)
   (pgmout son2 '(5 6 7 8) port)
   "ok")


(om::defmethod! tx-duo-vol ((vol1 integer ) (vol2 integer ) &optional (port 0))
   :initvals '( 127 127 0) 
   :indoc '("vol1"  "vol2" "port")
   :icon 137
   :doc  "volume--> TX1/4, TX5/8"
  (volume  vol1 '(1 2 3 4) port)
  (volume  vol2 '(5 6 7 8) port)
  "ok")



(defun TXtun1 (tune  canal port) 
"accord TX: 0= bécarre  21= +1/8  42= +1/4  63= + 3/8"
  (midi-o (list 240 67 (+ 15 canal) 4 64 (+ 64 tune) 247) port))


(om::defmethod! TXtune ((tunings t) (chans t ) &optional (port 0)) 
   :initvals '( '(0 21 42 63 0 21 42 63) '(1 2 3 4 5 6 7 8) 0) 
   :indoc '("tunings"  "chans" "port")
   :icon 137            
   :doc   "Sends global tuning parameters to a Yamaha TX-816. The value <tuning>
will be sent to the midi channel specified, <chans>. If <chans> is a list 
the tuning will be sent to all listed channels."
  (let ((port (create-list 8 port)))
    (cond ((not (consp tunings))
         (mapc #'(lambda (canal) (TXtun1 tunings canal  port)) (list! chans)))
        ((not (consp chans))
         (mapc #'(lambda (tun) (TXtun1 tun chans  port)) (list! tunings)))
        (t (mapc #'TXtun1 tunings chans  port) ) ) ))



(defun gen-TXtun1 (tune  canal) 
"accord TX: 0= bécarre  21= +1/8  42= +1/4  63= + 3/8"
  (list 240 67 (+ 15 canal) 4 64 (+ 64 tune) 247))


(om::defmethod! gen-TXtune ((tunings integer) (chans integer )) 
   :initvals '( '(0 21 42 63 0 21 42 63) '(1 2 3 4 5 6 7 8)) 
   :indoc '("tunings"  "chans")
   :icon 136            
   :doc   "Generates tuning parameters to a Yamaha TX-816. Useful for maquettes"
(gen-TXtun1 tunings chans))


(om::defmethod! gen-TXtune ((tunings list) (chans list )) 
(flat (loop for tune in tunings
      for chan in chans
      collect (gen-TXtun1 tune chan))))


(om::defmethod! gen-TXtune ((tunings integer) (chans list )) 
(flat (loop for chan in chans
      collect (gen-TXtun1 tunings chan))))





(om::defmethod! tx-duo-tune ((tune1 integer ) (tune2 integer ) &optional (port 0))
   :initvals '( 8 8 0)
   :indoc '("tune1"  "tune2" "port")
   :icon 137              
   :doc   "tune (4 ou 8) --> TX1/4, TX5/8"
  (txtune  (if (= tune1 4) '(0 0 43 43) '(0 21 42 63))  '(1 2 3 4) port )
  (txtune  (if (= tune2 4) '(0 0 43 43) '(0 21 42 63))  '(5 6 7 8) port)
  "ok")


(om::defmethod! tx-pgm ((son1 integer ) (son2 integer ) (son3 integer ) (son4 integer )
                (son5 integer ) (son6 integer ) (son7 integer ) (son8 integer ) &optional (port 0))
   :initvals '( 1 1 1 1 1 1 1 1 0)
   :indoc '("son1"  "son2" "son3" "son4" "son5" "son6" "son7" "son8" "port")
   :icon 137              
  (pgmout (list son1 son2 son3 son4 son5 son6 son7 son8) '(1 2 3 4 5 6 7 8) port)
  "ok")


; ==========================   PORTS    ==================================================




; methodes à utiliser en conjonction avec sel-maq et channel->voice pour transformer une maquette en mseq
; ameliorer pour que l'attribution du port dans canal+->port se fasse pour chaque note

(om::defmethod! port->canal+ ((self container) )
  :initvals '(nil )
  :icon 136
  :doc  "transforme infos de port + canal en notation canal+  = port*16 + (canal - 1)"
        (ch-modif self '= (om+ (om- (lchan self) 1) (om* 16 (get-port self)))     'lchan))

; nom abrégé
(om::defmethod! pc+ ((self container) )
  (port->canal+ self))


#|  (om::defmethod! canal+->port ((self multi-seq) )

  :initvals '(nil )
  :icon 136
  :doc  "transforme info canal+  en notation  port et canal midi. Suppose que les notes de 
chaque chord possèdent le même port"
(let ( (lport (om// (lchan self) 16))

  (self (ch-modif self '= (om+ 1 (om-modulo (lchan self) 16)) 'lchan)))

(loop for chseq in (chord-seqs self)
          for lpcs in lport
          do (loop for ch in (chords chseq)
                   for lpch in lpcs
                   do (setf (LPort ch)  (car lpch)) 
                   finally (return self))
          finally (return self))))
|#


(om::defmethod! canal+->port ((self chord-seq) )
  :initvals '(nil )
  :icon 136
  :doc  "transforme info canal+  en notation  port et canal midi. Suppose que les notes de 
chaque chord possèdent le même port"
(let ( (ports (om// (lchan self) 16))
       (self (ch-modif self '= (om+ 1 (om-modulo (lchan self) 16)) 'lchan)))
  (setf (LPort self) ports)))


(om::defmethod! canal+->port ((self multi-seq) )
  (mki 'multi-seq
     :chord-seqs (mapcar 'canal+->port (chord-seqs self) )))






; nom abrégé
(om::defmethod! cp+ ((self container) )
  (canal+->port  self))



; ---------- permet de jouer plusieurs ports midi dans un multiseq -------

#|   (defmethod* PrepareToPlay ((self multi-seq) at &key  approx port interval)
   (setf port (verify-port port))
   (loop for sub in (inside self) 
         for myport in *myports*  do
         (let ((objstart (+ at (offset->ms sub))))
           (if interval
             (let ((newinterval (interval-intersec interval 
                                                   (list objstart (+ objstart (get-obj-dur sub))))))
               (when newinterval
                 (PrepareToPlay sub objstart 
                                :approx approx 
                                :port myport
                                :interval interval)))
             (PrepareToPlay sub objstart 
                            :approx approx 
                            :port myport)))))

(om::defmethod! gport ((ports list))
  :initvals '('( 0 0 0 0 1 1 1 1))
   :doc "assign ports to the chord-seqs of the multi-seqs. 
    Changes a global variable : all multi-seqs will be effected"
   :icon 136 
  (setf *myports* ports))

|#



(defun pb-range-one (canal range )
  (midi-o (list 240 67 (+ 15 canal) 4 4 0 247))          ; step à 0 
  (midi-o (list 240 67 (+ 15 canal) 4 3 range 247)))


(om::defmethod! pb-range ((range integer)  (canaux t)) 
   :initvals '( 12 '(1 2 3 4 5 6 7 8) )
   :indoc '("range"  "canaux")
   :icon 136
   :doc "fixe l'intervalle du pitch bender, en 1/2 tons"
   (car-mapcar #'pb-range-one canaux range ) )




(defun ctrl1 (canal num val)
  (midi-o (list (+ 175 canal) num val)))

(om::defmethod! ctrlout ((num integer) (val integer) (canaux t ))
   :initvals '(1 127 '(1 2 3 4 5 6 7 8))
   :indoc '("num"  "val" "canaux")
   :icon 136
   "valeur de sortie d'un contrÙleur"
   (car-mapcar #'ctrl1  canaux num val ) )


; ........................................................
; jeu d'un spectre dynamique avec pitch-bend et ctrl 7

(defvar *range)

(om::defmethod! accord-moyen ((l-fqs list) (nbhq integer))
   :indoc '("l-fqs" "nbhq")
   :initvals '('(1 2) 1)
   :icon 136
   :doc  ""
   (let ((l-midics (f->mc (list-pos l-fqs 0 (1- nbhq) 1)))  res l-ecarts) 
     (dolist  (hq l-midics)
       (let ((valmoy (om-round (average (list (list-max  hq) (list-min hq)) 1) -2)))
         (push valmoy res)
         (push (list-max (om-abs (om- hq valmoy))) l-ecarts)))
     
     (setf *range (ceiling  (list-max  l-ecarts) 100))
     (nreverse res)))




(defun pb-val (l-midics  valmoy densite)
  (x-append "pb" (om-round 
                  (densifier (om* (om/ (om- l-midics valmoy) *range) 81.90) densite))))

(defun ctrl-val (l-vels densite)
  (let ((l-vels (om-round (densifier l-vels densite)) ) res  )
   (dolist  (n l-vels)
              (push 7  res)
              (push n res))
  (x-append "ctrl" (nreverse res))))


(om::defmethod! pbend-ctrl7 ((l-fqs list) (l-amps list)
                                (nbhq integer) (acmoyen list)
                                (densite integer) (curve integer))
   
   :indoc '("l-fqs" "l-amps" "nbhq" "acmoyen" "densite" "curve")
   :initvals '('(220 440) '(80 80) 1 '(2 2) 1 75)
   :icon 136
   :doc  "entrées: 
l-fqs = liste des fréqs
l-amps = liste des amplitudes
nbhq = nombre de partiels considérés
acmoyen = accord moyen (rendu par la boîte de ce nom)
densité = nb d'interpolations entre les valeurs pb et ctrl7
curve = déformation échelle des amplitudes en %
(pour passer de linéaire à vélocités midi)  50% = linéaire
> 50% = exponentielle"
   
   (let ((l-midics (f->mc (list-pos l-fqs 0 (1- nbhq) 1)))
         (l-vels   (om-round (ll-deformer% 
                              (ll-scaling (list-pos l-amps 0 (1- nbhq) 1) 0 127) curve)))
         res )
     (dotimes   (hq nbhq)
       (push (list 
              (pb-val (nth hq l-midics) (nth hq acmoyen) densite)
              (ctrl-val (nth hq l-vels) densite))
             res))
     (nreverse res)))

; =============================== objets ================================

; tests


;(make-instance 'chord-seq
  ;    :lmidic chords
 ;     :lonset lonset 
 ;     :legato 100
; (mki 'chord
;       :LMidic 
;       :Lvel 
;       :Loffset 
;       :Ldur 
 ;      :Lchan 
;       :lport



(defun set-chord-slot (accord slot value)
  (cond  ((eq slot 'lmidic)  (setf (lmidic accord) value))
         ((eq slot 'ldur) (setf (ldur accord) value))
         ((eq slot 'loffset) (setf (loffset accord) value))
         ((eq slot 'lchan) (setf (lchan accord) value))
         ((eq slot 'lvel) (setf (lvel accord) value))
        ((eq slot 'lport) (setf (lport accord) value)) ))



(defun ch-dur (chord)
  (list-max (om+ (ldur chord) (loffset chord))))


(om::defmethod! ch-length ((obj chord))
  :initvals '(nil )
  :icon 136
  :doc  "nb notes d'un accord - si obj est un ch-seq, liste  nb de notes de chaque accord "
  (length (lmidic obj)))

(om::defmethod! ch-length ((obj chord-seq))
  (mapcar 'length (lmidic obj)))





(om::defmethod! obj-dur ((obj chord))
  :initvals '(nil )
  :icon 136
  :doc  "duree d'un objet - tenant compte de offset et dur "
  (list-max (om+ (ldur obj) (loffset obj))))

(om::defmethod! obj-dur ((obj chord-seq))
(list-max (om+ (mapcar 'obj-dur (get-chords obj)) (butlast (lonset obj)))))


(om::defmethod! obj-dur ((obj multi-seq))
(list-max (mapcar 'obj-dur (chord-seqs obj))))



(om::defmethod! canaux ((obj container))
  :initvals '(nil )
  :icon 136
  :doc  "liste des canaux presents dans l'objet "
  (sort (remove-dup (remove nil (flat (lchan obj))) 'eq 1) '<))



(om::defmethod! obj-minmax ((obj t))
  :initvals '(nil )
  :numouts 2
  :icon 136
  :doc  "midic minimal et maximal de l'objet "
  (values-list (list (list-min (lmidic obj)) (list-max (lmidic obj)))))
 




;(om::defmethod! selectf ((self container) (start number) (end t) &optional (track nil))
;:initvals '(nil 0 nil nil)
;  :icon 137
;  :doc "comme select, mais neutralise les C4 indésirables
; piste permet de choisir une seule piste (dans un multi-seq)
;Si self = maquette , extrait les objets dont offset est entre start et end"
;(cond  ((null piste) (filterC4 (select self start end)))
;       ((listp piste)
;        (filterC4 (select (mki 'multi-seq :chord-seqs (posn-match (chord-seqs self) piste)) start end)))
;       (t (filterC4 (select (nth piste (chord-seqs self)) start end))
;  )))
; correction 9-05  teste valeur de end

(om::defmethod! selectf ((self chord-seq) (start number) (end t) &optional (piste nil))
:initvals '(nil 0 nil nil)
  :icon 137
  :doc "comme select, mais neutralise les C4 indésirables
piste permet de choisir une seule piste (dans un multi-seq)
Si self = maquette , extrait les objets dont offset est entre start et end"

  (let ((end (if end (min end (last-elem (lonset self))) (last-elem (lonset self)))))
  
    (filterC4 (select self start end))))

(om::defmethod! selectf ((self multi-seq) (start number) (end t) &optional (piste nil))
:initvals '(nil 0 nil nil)
  (let ( (piste  (if (null piste) (arithm-ser 0 (length-1 (chord-seqs self)) 1) piste))
        res)
    
    (if (listp piste)
      (progn (dolist (p piste)
        (push (selectf (nth-obj self p)  start end piste) res))
        (mki 'multi-seq :chord-seqs (reverse res)))
      (selectf (nth piste (chord-seqs self)) start end piste))))

(om::defmethod! selectf ((self ommaquette) (start t) (end t) &optional (piste nil))
  (let* ((objets (temporalboxes self))  (start (if (eq start nil) 0 start))
        (end (if (eq end nil) (l-max (mapcar 'offset objets)) end))
        res)
(loop for ob in objets
      do (if (<> (offset ob ) start end '<>=) (push (value ob) res))
      finally (return (reverse res)))))

;====Rename selectf as select-filt 28-06-2007=========================

(om::defmethod! select-filt ((self chord-seq) (start number) (end t) &optional (track nil))
   :initvals '(nil 0 nil nil)
  :icon 137
  :doc "Returns the selected excerpt between <start> and <end> while neutralizing the undesired C4s. The optional <track> allows to specify a single track in a multi-seq. In case <self> is a maquette, the objects possessing an offset between <start> and <end> will be extracted."

  (selectf self start end track))

(om::defmethod! select-filt ((self multi-seq) (start number) (end t) &optional (track nil))

  (selectf self start end track))

(om::defmethod! select-filt ((self ommaquette) (start t) (end t) &optional (track nil))

  (selectf self start end track))

;====================================================================


(om::defmethod! seq-extract ( (chseq chord-seq) (begin integer) (end integer))
  :initvals '(nil 0 3)
  :icon 136
  :doc "Extracts part of a chord sequence between <begin> and <end>. N.B. <begin>=0 indicates that the selection starts with the very first chord of the original sequence."
  
  (mki 'chord-seq
       :lmidic (posn-match  (get-chords chseq) (arithm-ser begin end 1))
       :lonset (om- (posn-match  (lonset chseq) (arithm-ser begin end 1)) (nth deb (lonset chseq)))
       :legato (legato chseq)))


(om::defmethod! nth-obj ( (obj chord-seq) (nth t))
  :initvals '(nil 0)
  :icon 136
  :doc "Extracts the nth chord from a chord sequence object, or the nth chord sequence from a multi sequence object, or the nth object from a maquette. <nth> may be a liste.
N.B. <nth>=0 indicates the first object."
  
  (posn-match  (get-chords obj) nth))

(om::defmethod! nth-obj ( (obj multi-seq) (nth t))
  (posn-match  (chord-seqs obj) nth))

; --- nth-obj pour maquettes

(om::defmethod! nth-obj ( (obj ommaquette) (nth integer))
   (let ((objets (temporalboxes obj)))
         (car  (value (posn-match (second (sort-table (list (mapcar 'offset objets) objets) 0)) nth)))))


(om::defmethod! nth-obj ( (obj ommaquette) (nth list))
   (let ((objets (temporalboxes obj)))
         (mapcar  'value (posn-match (second (sort-table (list (mapcar 'offset objets) objets) 0)) nth))))

(om::defmethod! last-n ( (obj chord-seq) (nth number))
  (let ((long (1- (length (lmidic obj)))))
  (posn-match  (get-chords obj) (arithm-ser (- long (1- nth)) long 1 ))))



; tester encore
(om::defmethod! sort-chords ((object chord-seq))
  :initvals '(nil )
  :icon 136
  :doc "remet les accords d'un chord-seq dans l'ordre des onset
si object = accord : remet les midic dans l'ordre ascendant"
(let ((lchords (sort-table  (list (get-chords object) (butlast (lonset object))) 1)))
    (mki 'chord-seq
         :LMidic    (first lchords)
         :lonset (x-append (second lchords) (max (last-elem (lonset object)) 
                                                 (+ (last-elem (second lchords) )
                                                    (list-max (ldur (last-elem (first lchords))))
                                                    (list-max (loffset (last-elem (first lchords)))))))
         :legato (legato object))))



(om::defmethod! sort-chords ((object chord))

(notes->chord (mat-trans (sort-table  (mat-trans (chord->notes object)) 0))))





(om::defmethod! chord->notes ((self chord))
  :initvals '(nil )
  :icon 136
  :doc  "extrait les notes d'un chord. Les notes sont ici de simples listes 
(pitch vel offset dur chan port) "
  (loop for pitch in (lmidic self)
        for dur in (ldur self)
        for vel in (lvel self)
        for offset in (loffset self)
        for chan in (lchan self)
        for port in (lport self)
        collect  (list   pitch vel offset dur chan port)) )


(om::defmethod! notes->chord ((notes list))
  :initvals '(nil )
  :icon 136
  :doc  "Reconstruit un accord à partir de simples listes 
(pitch vel offset dur chan port) "
(let ((notes (remove nil notes)))
  (mki 'chord
       :LMidic (mapcar 'first notes)
       :Lvel (mapcar 'second notes)
       :Loffset (mapcar 'third notes)
       :Ldur (mapcar 'fourth notes)
       :Lchan (mapcar 'fifth notes)
       :Lport (mapcar 'sixth notes))))




; from chord-seq->mf-info  but here pitch = midicent  legato = 0

(om::defmethod! seq->notes ((self chord-seq))
   :initvals '(nil )
   :icon 136
   :doc  "extrait les notes d'un ch-seq (multi-seq). Les notes sont ici de simples listes 
(pitch (+ onset offset) dur vel chan) "
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
                       collect (list  pitch (+ onset offset) dur vel chan port)) ))


(om::defmethod! seq->notes ((self multi-seq))
(mapcar 'seq->notes (chord-seqs self)))


(om::defmethod! notes->seq ((lnotes list))
  :initvals '( '((6000 0 1000 80 1) (6900 0 1000 80 1)))
  :icon 136
  :doc  "crée un chord-seq à partir de  notes (listes
(pitch onset dur vel chan port)) "
(if (atom (first (first lnotes))) (notes->chord-seq lnotes) (notes->multi-seq lnotes)))


(om::defmethod! notes->chord-seq ((lnotes list))
   :initvals '( '((6000 0 1000 80 1 0) (6900 0 1000 80 1 0)))
   :icon 136
   :doc  "crée un chord-seq à partir de  notes (listes
(pitch onset dur vel chan port)) "
   (let* ( (lnotes (mat-trans (sort-table (mat-trans lnotes) 1))) (lastnote (last-elem lnotes)) lchords)
     (dolist  (note lnotes)
       (push (mki 'chord
                  :LMidic (list (first note))
                  :Ldur (list (third note))
                  :Loffset '(0)
                  :Lchan (list (fifth note))
                  :Lvel (list (fourth note)) 
                  :lport (list (sixth note)) )
             lchords))
     (mki 'chord-seq
          :LMidic (nreverse lchords)
          :lonset (x-append (mapcar 'second lnotes) (+ (second lastnote) (third lastnote)))
          :legato 0)))





(om::defmethod! notes->multi-seq ((lnotes list))
  :initvals '( '(((6000 0 1000 80 1) (6900 0 1000 80 1)) ((6000 0 1000 80 1) (6900 0 1000 80 1))))
  :icon 136
  :doc  "crée un multi-seq à partir de  notes (listes
(pitch onset dur vel chan)) "
(mki 'multi-seq :chord-seqs
     (mapcar 'notes->seq lnotes)))
  

(om::defmethod! multi-seq-vide ((ntracks integer))
  :initvals '(5)
  :icon 136
  :doc  "Creates an empty multi sequence object with <ntracks> tracks."

(mki 'multi-seq :chord-seqs
     (mapcar 'notes->seq (create-list ntracks '((6000 0 10 0 1 999))))))



; explosion est la nv fct générale . explode-seq est gardé pour compatibilité

(om::defmethod! explode-seq ((obj chord-seq)  )
:initvals '(nil  )
  :icon 136
  :doc  "explose les accords d'un chord-seq (multi-seq) en accords ne 
contenant qu'une seule note . Utile aussi pour changer les modif d'offsets en 
onsets modifiés"
(notes->seq  (seq->notes obj)))

(om::defmethod! explode-seq ((obj multi-seq) )
(mki 'multi-seq :chord-seqs
     (loop for c in (chord-seqs obj)
           collect (if (null (seq->notes c)) (notes->seq '((6000 0 10 0 1 999)) )   ; note bidon
                       (explode-seq c )))))


(om::defmethod! explosion ((obj chord)  )
:initvals '(nil  )
  :icon 136
  :doc  "explose les notes d'un chord , chord-seq ou multi-seq en accords ne 
contenant qu'une seule note  . Change les offsets en onsets "
(let ((lnotes (chord->notes obj)))
  
      (mki  'chord-seq 
      :lmidic (mapcar  'first lnotes)
      :lvel  (mapcar 'second lnotes)
      :lonset (mapcar 'third lnotes)
      :ldur  (mapcar 'fourth lnotes)
      :lchan (mapcar 'fifth lnotes)
      :lport (mapcar 'list! (mapcar  'sixth lnotes))    ; mapcar 'list!   parce que lport (ch-seq) attend une liste de listes
      :legato 0)))




(om::defmethod! explosion ((obj chord-seq)  )
(explode-seq obj))

(om::defmethod! explosion ((obj multi-seq)  )
(explode-seq obj))




(om::defmethod! implosion ((obj chord-seq)  )
:initvals '(nil  )
  :icon 136
  :doc  "transforms a chord-seq (or multi-seq) into a chord  . Change onsets into offsets. Useful for iterations, trills, etc...
as it should simplify internal representation . "

(let ((lnotes (seq->notes obj)))
  
      (mki  'chord 
      :lmidic (mapcar  'first lnotes)
      :lvel  (mapcar 'fourth lnotes)
      :loffset   (mapcar 'second lnotes)                                              
      :ldur  (mapcar 'third lnotes)
      :lchan (mapcar 'fifth lnotes)
      :lport  (mapcar  'sixth lnotes)
      )))


(om::defmethod! implosion ((obj multi-seq))
  :initvals '(nil)
  :icon 136
  (implosion (mixer obj nil)))






(om::defmethod! lier ((obj chord-seq) &optional (legato 100))
  :initvals '(nil  100)
  :icon 137
  :doc  "chord-seq : chaque note de chaque accord dure jusqu'à l'onset suivant
chord : chaque note dure jusqu'à la note suivante (si offsets <> 0)
arg optionnel legato : durée égale à un % de l'intervalle de temps ainsi défini (100 = legato parfait,  
> 100 = tuilage , < 100 = staccato "
  (let* ( (interv  (butlast (x->dx (lonset obj))))
         (res (loop for ch in (butlast (chords obj))
                for in in interv
                collect (ch-modif ch '= (om-round (om/ (om* legato in) 100)) 'ldur))))
    (mki 'chord-seq :lmidic (x-append res (last-elem (chords obj))) :lonset (lonset obj))))


; corr  11/10/03     le dernier objet reste inchangé (précédemment, sa durée était la durée de son élément le plus long :
;(setf (ldur res)  (x-append (om-round (om/ (om* legato (butlast (x->dx (lonset obj)))) 100)) (last-elem (ldur obj))))
 ;   res))



(om::defmethod! lier ((obj chord) &optional (legato 100))

(let* ((lparam (sort-table (mat-trans (chord->notes obj)) 2))
       (ldurres  (x-append (om-round (om/ (om* legato  (x->dx (nth 2 lparam)))) 100) (last-elem (nth 3 lparam))))
       (lparam (list (nth 0 lparam) (nth 1 lparam) (nth 2 lparam) ldurres (nth 4 lparam))))
  (notes->chord (mat-trans lparam))))


(om::defmethod! lier ((obj multi-seq) &optional (legato 100))
  (mki 'multi-seq
     :chord-seqs (car-mapcar 'lier (chord-seqs obj) legato)))


;====Rename lier as slur 28-06-2007========================================

(om::defmethod! lier ((obj chord-seq) &optional (legato 100))
  :initvals '(nil  100)
  :icon 137
  :doc  "Adds a slur to the notes within an object <obj>.
In case <obj> is a chord sequence object: Each note of every chord will will be modified to last exactly until the subsequent onset.
In case <obj> is a chord object: Each note will be modified to last exactly until the subsequent note occurs (if onsets are <>0).

Optional: <legato>=percentage of the total duration (i.e. 100% means perfect legato; > 100 means overlap, <100 means staccato)."

  (lier obj legato))

(om::defmethod! slur ((obj chord) &optional (legato 100))
  :icon 137
  (lier obj legato))

(om::defmethod! slur ((obj multi-seq) &optional (legato 100))
  :icon 137
  (lier obj legato))

;==========================================================================

(om::defmethod! lier* ((obj t) (muldur number) &optional (legato 100))
   :initvals '(nil  1 100)
  :icon 137
  :doc  "Adds a slur and a duration stretch to the notes of an object <obj>. The stretch is obtained by a multiplication of the duration values, leading to an accelerando or rallentando)."

  (lier (om* obj muldur) legato))


;====Rename lier* as slur-stretch 28-06-2007================================

(om::defmethod! slur-stretch ((obj t) (muldur number) &optional (legato 100))
  :initvals '(nil  1 100)
  :icon 137
  :doc
  "Adds a slur and a duration stretch to the notes of an object <obj>. The stretch is obtained by a multiplication of the duration values."
  
  (lier* obj muldur legato))

;============================================================================

(om::defmethod! lier-nth ((obj multi-seq) (nth t) &optional (legato 100)  )
  :initvals '(nil 0 100)
   :icon 137
   :doc  "Adds a slur to the notes within the <nth> chord sequence objects within a multi sequence object <obj>.
<nth> may be a number or a list."

(let ((long (1- (length (chord-seqs obj)))) (nth (list! nth))  res)
    (for  (n 0 1 long)
      (push (if (included? n nth) (lier (nth-obj obj n) legato) (nth-obj obj n)) res))
  (mki 'multi-seq
     :chord-seqs  (nreverse res))))


;====Rename lier-nth as n-slur 28-06-2007====================================

(om::defmethod! lier-nth ((obj multi-seq) (nth t) &optional (legato 100))
  :initvals '(nil 0 100)
   :icon 137
   :doc  "Adds a slur to the notes within the <nth> chord sequence objects within a multi sequence object <obj>.
<nth> may be a number or a list."

   (lier-nth obj nth legato))

;============================================================================








(om::defmethod! ch-modif ((object t) (fct t) (arg t) (slot t)) 
  :initvals '(nil '+ 100 'lmidic)
   :menuins '((3 (("midic" 'lmidic) ("vel" 'lvel) ("dur" 'ldur) ("offset" 'loffset) ("chan" 'lchan) ("port" 'lport) )))
   :indoc '("object" "function" "argument" "slot")
   :icon 136
   :doc  "remplace les valeurs du <slot> par ( <fct>  <ancienne valeur>  <arg>) 
Si fct est ''  =  ''   remplace ancienne valeur par <arg>
Accepte accord, chordseq, multiseq.
Si chseq et arg=liste,  la liste des arg et celle des accords sont
déroulées de façon synchrone. "


   (let ((res (clone object)))
     (set-chord-slot  res slot
                      (if  (eq fct '=)   
                        (if (atom arg) (create-list (length (lmidic object)) (om-round arg)) (om-round arg))        
                        (om-round (tm-oper fct (funcall slot res ) arg))))
     res))


(om::defmethod! ch-modif ((object list) (fct t) (arg t) (slot t)) 
  (car-mapcar  'ch-modif object  fct arg slot))

; a cause mauvaise definition de setf lport

(om::defmethod! ch-modif ((object chord-seq) (fct t) (arg t) (slot t)) 
   (let ((res (clone object))
         (arg  (if (and (eq slot 'lport) (one-elem arg)) (create-list (mapcar 'length (lmidic object)) arg) arg)))
     (set-chord-slot  res slot
                      (if  (eq fct '=)   
                        (if (atom arg) (create-list (length (lmidic object)) (om-round arg)) (om-round arg))        
                        (om-round (tm-oper fct (funcall slot res ) arg))))
     res))

; corr  26-9-03 

(om::defmethod! ch-modif ((object multi-seq) (fct t) (arg number) (slot t))
(mki 'multi-seq
     :chord-seqs (loop for chseq in (chord-seqs object)
                       collect (ch-modif chseq fct arg slot))))


(om::defmethod! ch-modif ((object multi-seq) (fct t) (arg list) (slot t))
  (mki 'multi-seq
     :chord-seqs (loop for chseq in (chord-seqs object)
                       for a in arg
                       collect (ch-modif chseq fct a slot))))




(defun set-chord-slot (accord slot value)

  (cond  ((eq slot 'lmidic)  (setf (lmidic accord) value))
         ((eq slot 'ldur) (setf (ldur accord) value))
         ((eq slot 'loffset) (setf (loffset accord) value))
         ((eq slot 'lchan) (setf (lchan accord) value))
         ((eq slot 'lvel) (setf (lvel accord) value))
         ((eq slot 'lport) (setf (lport accord) value)) ))





; ajout options  14-9-02

(om::defmethod! ch-distor ((object t) (min number) (max number) (mode t)  &optional (minin nil) (maxin nil))
  :initvals '(nil 3600 8400 'midic nil nil)
   :menuins '((3 (("midic" 'midic) ("freq" 'freq) ("multipl" 'multipl)  )))
   :indoc '("object" "nv minimum" "nv maximum" "mode" "min ref" "max ref")
   :icon 137
   :doc  "distorsion de hauteur. Indiquer nouvelles hauteurs minimales et maximales.
Option : donner des hauteurs min et max de reference (cf fonction om-scale)
Si mode 'midic' : distorsion calculée sur midic
Si mode 'freq' : distorsion calculée sur fq (donner néanmoins les min et max en midic)
Si mode 'multipl' : distorsion calculée sur fq, min et max sont des multiplicateurs de 
la frequence (ie. si min et max = 1, rien ne se passe, si min = 2, la hauteur la plus basse est 
remontée d'une octave, etc...
Accepte accord, chordseq, multiseq, liste chseq."
 (let* ((res (clone object))
       (midics (lmidic object) )
       (minmidics (if (null minin) (list-min midics) minin))
       (maxmidics (if (null maxin) (list-max midics) maxin)))

   (set-chord-slot  res 'lmidic (om-round 
                     (cond ((eq mode 'midic) (om-scale midics min max minmidics maxmidics))
                           ((eq mode 'freq) (f->mc (om-scale (mc->f midics) (mc->f min) (mc->f max)
                                                             (mc->f minmidics) (mc->f maxmidics))))
                           ((eq mode 'multipl) (f->mc (om-scale (mc->f midics) (* min (mc->f minmidics))
                                         (* max (mc->f maxmidics)) (mc->f minmidics) (mc->f maxmidics)))))))
res))





(defun multi-distor (object  min max minmidics maxmidics mode )
(let* ((res (clone object))
         (midics (lmidic object) ))
   (set-chord-slot  res 'lmidic (om-round 
                     (cond ((eq mode 'midic) (om-scale midics min max minmidics maxmidics))
                           ((eq mode 'freq) (f->mc (om-scale (mc->f midics) (mc->f min) (mc->f max)
                                                             (mc->f minmidics) (mc->f maxmidics))))
                           ((eq mode 'multipl) (f->mc (om-scale (mc->f midics) (* min (mc->f minmidics))
                                         (* max (mc->f maxmidics)) (mc->f minmidics) (mc->f maxmidics)))))))
res))


; liste chseq
(om::defmethod! ch-distor ((object list) (min number) (max number) (mode t) &optional (minin nil) (maxin nil))
  (let* ((midics (mapcar 'lmidic  object))
       (minmidics (if (null minin) (list-min midics) minin))
       (maxmidics (if (null maxin) (list-max midics) maxin)))
(car-mapcar 'multi-distor  object  min max 
                             minmidics maxmidics mode)))


(om::defmethod! ch-distor ((object multi-seq) (min number) (max number) (mode t) &optional (minin nil) (maxin nil))
  (mki 'multi-seq
     :chord-seqs (ch-distor (chord-seqs object)  min max  mode minin maxin)))



; interpolation d'accords

(om::defmethod! ch-interpol ((begin chord) (end chord) (samples integer) (curve float) (mode t))
 :initvals '(nil nil 4 1.0 'midic)
   :menuins '((4 (("midic" 'midic) ("freq" 'freq)   )))
   :icon 136
   :doc  "interpole des accords (de même nombre de notes) ; mode : interpolation par midic ou par freq
dur et vel sont aussi interpolés
canaux et ports = canaux et ports accord initial
rend un chord-seq"
 (let ((res (mki 'chord-seq
                     :LMidic (om-round (if (eq mode 'midic) (interpolation (lmidic begin) (lmidic end)  samples curve)
                                 (fq-interpol (lmidic begin) (lmidic end)  samples curve)))
                     :Ldur  (om-round (interpolation (ldur begin) (ldur end)  samples curve))
                     :LOffset (om-round (interpolation (loffset begin) (loffset end)  samples curve))
                     :Lchan  (create-list samples (lchan begin))
                     :Lvel (om-round(interpolation (lvel begin) (lvel end)  samples curve))
                     :lonset (dx->x 0 (mapcar 'list-max (om-round (interpolation (ldur begin) (ldur end)  samples curve))))
                     )))

(set-chord-slot  res 'lport (create-list samples (lport begin)))))
                     
          




(om::defmethod! lonset-modif ((object chord-seq) (fct t) (arg t) &optional (deb nil) (fin nil)) 
  :initvals '(nil '* 2  nil nil)
  :icon 137
  :doc  "remplace les valeurs du slot 'lonset' par ( <fct>  <ancienne valeur>  <arg>) 
Si fct est ''  =  ''   remplace ancienne valeur par <arg>
Si  arg=liste,  la liste des arg et celle des accords sont
déroulées de façon synchrone
Accepte chseq et mseq
Optional : permet de ne traiter qu'une sélection, entre deb et fin"
(if (null deb)
  (mki 'chord-seq
       :LMidic (get-chords object)
       :lonset (if  (eq fct '=)   
                 arg        
                 (om-round (tm-oper fct (lonset object ) arg)))
       :legato (legato object))
  (lonset-modif-sel object fct arg deb fin)))



(om::defmethod! lonset-modif ((object multi-seq) (fct t) (arg number) &optional (deb nil) (fin nil)) 
 (if (null deb)
   (mki 'multi-seq
       :chord-seqs
       (car-mapcar 'lonset-modif (chord-seqs object) fct arg))
   (lonset-modif-sel object fct arg deb fin)))


(defun lonset-modif2 (object arg fct deb fin) 
(lonset-modif object fct arg deb fin))

(om::defmethod! lonset-modif ((object multi-seq) (fct t) (arg list) &optional (deb nil) (fin nil)) 
(mki 'multi-seq
       :chord-seqs
       (double-mapcar 'lonset-modif2 (chord-seqs object) arg fct deb fin )))





(om::defmethod! lonset-modif-sel ((object chord-seq) (fct t) (arg t) (deb integer) (fin t) ) 
  :initvals '(nil '* 2  1000 2000)
  :icon 136
  (let ((fin (if (null fin) (last-elem (lonset object)) fin)) newonsets)
    (dolist (att (lonset object))
      (if (<>  att deb fin '<>=)
        (push  (if  (eq fct '=)   
                arg        
                (om-round (funcall fct att arg)))  newonsets)
        (push att newonsets)))
    (mki 'chord-seq
         :LMidic (get-chords object)
         :lonset (nreverse newonsets)
         :legato (legato object))))
     

(om::defmethod! lonset-modif-sel ((object chord-seq) (fct t) (arg t) (deb integer) (fin t) ) 
  :initvals '(nil )
  :icon 136
  (mki 'multi-seq
       :chord-seqs
       (loop for seq in (chord-seqs object)
             collect (lonset-modif-sel seq fct arg deb fin))))





(om::defmethod! reverse-obj ((object chord) ) 
  :initvals '(nil )
  :icon 136
  (setf (loffset (clone object)) (nreverse (loffset object))))


(om::defmethod! reverse-obj ((object chord-seq) ) 
  :initvals '(nil )
  :icon 136
(reverse-chseq object (first (lonset object))))


(om::defmethod! reverse-obj ((object multi-seq) ) 

(let ((lastatt (list-max (flat (mapcar 'butlast (lonset object))))))

  (mki 'multi-seq
       :chord-seqs 
       (loop for seq in  (chord-seqs object)
             collect (reverse-chseq seq (- lastatt (last-elem (butlast (lonset seq )))))))))


(defun reverse-chseq (object deb)
(let ((newonsets (x-append (dx->x deb (nreverse (butlast (x->dx (lonset object)))))
                           (last-elem (lonset object)))))
(mki 'chord-seq
         :LMidic (nreverse (mapcar 'reverse-obj (get-chords object)))
         :lonset newonsets
         :legato (legato object))))












(defun chord-filter1 (accord  deb fin numer denom )
  (let* ((liste (arithm-crible deb (min fin (1- (length (lmidic accord)))) numer denom))) 
    (mki 'chord
       :LMidic (posn-match (lmidic accord) liste)
       :Ldur (posn-match (ldur accord) liste)
       :Loffset (posn-match (loffset accord) liste)
       :Lchan (posn-match (lchan accord) liste)
       :Lvel (posn-match (lvel accord) liste)
       :Lport (posn-match (lport accord) liste)
)))

(om::defmethod! ch-filter ((objet chord) (deb integer) (fin integer) &optional (numer 1) (denom 1))
  :initvals '((make-instance 'chord) 1 2 1 1)
   :indoc '("object" "début" "fin" "numérateur" "dénominateur")
   :icon 137
   :doc  "ne garde de l'objet accord que les notes dont le numero
d'ordre est compris  entre deb et fin
options: filtrage en peigne : garder 
numer sur denom "
  (chord-filter1  objet deb fin numer denom  ))


(om::defmethod! ch-filter ((objet chord) (deb integer) (fin null) &optional (numer 1) (denom 1))
(ch-filter  objet deb (1- (length (lmidic objet))) numer denom  ))



(om::defmethod! ch-filter ((objet chord-seq) (deb integer) (fin integer) &optional (numer 1) (denom 1))
  (let* ((lchords (car-mapcar #'chord-filter1  (get-chords objet) deb fin numer denom  ))
         (res (objfromobjs lchords objet)))
         (setf (lonset res) (lonset objet))
         (setf (legato res) (legato objet))
    res))
    




; faire methode pour ch-seq et multi-seq  - pb : le filtrage peut rapporter des ch-seq vides


(om::defmethod! ch-test-filter ((obj chord) (fct t) (arg t) (slot t))
  :initvals '(nil '> 10800 'lmidic)
  :menuins '((3 (("midic" 'lmidic) ("vel" 'lvel) ("dur" 'ldur) ("offset" 'loffset) ("chan" 'lchan))))
  :icon 136
  :doc  "filtre les notes qui répondent au test - chord et ch-seq seulement "
  (let ((notes (chord->notes obj))
        (nbslot (position slot '(lmidic lvel loffset ldur lchan)))     
        res)
    (dolist (n notes)
      (if (not (funcall fct (nth nbslot n) arg) ) (push n res)))
    (notes->chord (nreverse res))))

(om::defmethod! ch-test-filter ((obj chord-seq) (fct t) (arg t) (slot t))
(let ((lchords (get-chords obj)) (lonsets (lonset obj)) reschord resonset)
  (for (i  0 1 (1- (length lchords)))
    (let ((ch (ch-test-filter (nth i lchords) fct arg slot)))
      (if (not (null (lmidic ch)))
        (progn (push ch reschord) (push (nth i lonsets) resonset)))))
    (mki 'chord-seq
         :Lmidic (nreverse reschord)
         :Lonset (nreverse resonset)
         :legato (legato obj))))



; faire methode liste d'accords
; le filtrage ne tient pas compte des velocités ; idéalement, il faudrait garder la note
; ayant la plus forte vélocité

(om::defmethod! ch-remdup ((obj chord) (approx integer) )
  :initvals '(nil 4)
   :icon 136
  :doc  "retire les notes de hauteur identique, selon l'approximation indiquée "
(let ((notes (chord->notes obj))
      (midics (cdr (approx-m (lmidic obj) approx))) 
      res)
 (dolist (n notes)
   (if (not (member (approx-m (first n) approx) midics))
     (push n res))
   (setf midics (cdr midics)))
 (notes->chord (nreverse res))))


(om::defmethod! ch-remdup ((obj chord-seq) (approx integer) )
(mki 'chord-seq
         :Lmidic (list! (car-mapcar  'ch-remdup (chords  obj) approx))  ;list! permet ch-seq ne comportant qu'un seul accord
         :Lonset (lonset obj)
         :legato (legato obj)))

(om::defmethod! ch-remdup ((object multi-seq)  (approx integer)  )
  (mki 'multi-seq
     :chord-seqs (car-mapcar 'ch-remdup (chord-seqs object)  approx)))

; nv version à tester  
; garde la plus forte vélocité, le + petit midi, la + longue durée, le + petit offset

(om::defmethod! ch-remdup ((obj chord) (approx integer) )
  :initvals '(nil 4)
  :icon 136
  :doc  "retire les notes de hauteur identique, selon l'approximation indiquée "
  (let ((notes (chord->notes obj))
        (midics (approx-m (lmidic obj) approx))
        lpos res)
    (dolist (n notes)
      (push  (positions midics (approx-m (first n) approx) )  lpos)
      )
    (dolist (pos (remove-dup lpos 'equal 1))
      (let* ((dupnotes (posn-match notes pos))
             (dpitch (first (first dupnotes)))
             (dvel (list-max (mapcar 'second dupnotes)))
             (doff (list-min (mapcar 'third dupnotes)))
             (ddur (list-max (mapcar 'fourth dupnotes)))
             (dchan (list-min (mapcar 'fifth dupnotes)))
             (dport (list-min (mapcar 'sixth dupnotes))))
        (push (list dpitch dvel doff ddur dchan dport) res)))
    (notes->chord  res)))





(om::defmethod! ch-vocoder ((object chord) (reservoir list) 
                            &optional (mode 'midic))
  :initvals '((make-instance 'chord) '(6000 6400) 'midic)
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freqs))))
   :indoc '("object" "réservoir" "mode")
   :icon 137
   :doc
   "Applies the structure of an object <object> onto a harmonic reservoir <reservoir>. It chooses those values from the harmonic reservoir which match the structure of <object> most closely. <object> may be a chord, chord-seq, multi-seq or a list of chord-seq. Because of the extended possibilities for the format of the <object> input, this is a slightly more elaborate function than vocoder."

(mki 'chord
       :LMidic (vocoder (lmidic object) reservoir mode)
       :Ldur (ldur object )
       :LOffset (loffset object )
       :Lchan (lchan object )
       :Lvel (lvel object )
       :Lport (lport object )
))

(om::defmethod! ch-vocoder ((object chord-seq) (reservoir list)
                            &optional (mode 'midic))


   (mki 'chord-seq 
        :lmidic (loop for ch in (chords object)
                                 collect (ch-vocoder ch reservoir mode))
        :lonset (lonset object)
        :legato (legato object)))



(om::defmethod! ch-vocoder ((object multi-seq)  (reservoir list) &optional (mode 'midic  ))
  (mki 'multi-seq
     :chord-seqs (ch-vocoder (chord-seqs object)  reservoir mode)))


(om::defmethod! ch-vocoder ((object list)  (reservoir list) &optional (mode 'midic) )
  (car-mapcar 'ch-vocoder object reservoir mode))




(om::defmethod! channel->micro ((object chord)  (approx integer) &optional (mode 'keep))
  :initvals '((make-instance 'chord) 4 'keep)
  :menuins '((1 (("1/4" 4) ("1/8" 8))) (2 (("Keep" 'keep) ("Reduce" 'reduce))))
  :icon 137
  :doc  "transforms midi channel informations into midicents; ie chan 1 -> natural,
chan 2 -> +25 cents, chan 3 -> + 50 cents, chan 4 -> + 75 cents (when approx = 1/8).
When approx = 1/4 : chan 1 and chan 2 -> natural, chan 3 & chan 4 -> + 50 cents
Mode : 'keep' = chan 5, 6, 7, 8 become chan 5 + midicent increment, and so forth
'reduce' = all channels become  chan 1 + midicent increment"
  (let ((lincr (if (= approx 8 ) (om-modulo (om- (lchan object) 1) 4)
                   (om* 2 (om// (om-modulo (om- (lchan object) 1) 4) 2))))
        (lcan (if (eq mode 'keep) (om+ 1 (om* 4 (om// (om- (lchan object) 1) 4)))
                  1)))
    (mki 'chord
         :LMidic (om+  (lmidic object) (om* lincr 25))
         :Ldur (ldur object )
         :LOffset (loffset object )
         :Lchan lcan
         :Lvel (lvel object )
         :Lport (lport object ))))

(om::defmethod! channel->micro ((object chord-seq)  (approx integer) &optional (mode 'keep))
  (let ((lincr (if (= approx 8 ) (om-modulo (om- (lchan object) 1) 4)
                   (om* 2 (om// (om-modulo (om- (lchan object) 1) 4) 2))))
        (lcan (if (eq mode 'keep) (om+ 1 (om* 4 (om// (om- (lchan object) 1) 4)))
                  1)))
    (mki 'chord-seq
         :LMidic (om+  (lmidic object) (om* lincr 25))
         :lonset (lonset object)
         :Ldur (ldur object )
         :LOffset (loffset object )
         :Lchan lcan
         :Lvel (lvel object )
         :Lport (lport object ))))


; comparer avec chseq->multi-seq
; ajout de la méthode port->canal+ et réciproquement pour répartir les ch-seq en fonction
; des ports et des canaux

#|   (om::defmethod! channel->voice ((obj multi-seq) (align t) &optional (canaux nil ))
  :initvals '(nil 50 nil)
  :icon 137
  :doc  "organise multi-seq  : chaque canal est mis sur une piste différente
La structure d'accords n'est pas gardée telle qu'elle etait dans l'objet d'origine, mais
reconstruite en fonction de la valeur de <align> (ms). Si align = nil, les accords ne
sont pas reconstruits .
Les pistes sont rangées dans l'ordre des canaux, sauf si on donne une liste ordonnées de canaux
(optional : canaux)"
(let ((lnotes (flat-once (seq->notes obj)))
      (canaux (if (null canaux) (canaux obj) canaux))
      lchseq)
  (dolist (chan canaux)
   (push (mkpiste1 lnotes chan 'remove) lchseq))
  (align-chords (mki 'multi-seq :chord-seqs  (nreverse (remove nil lchseq)) ) align)))
|#


(om::defmethod! channel->voice ((obj multi-seq) (align t) &optional (canaux nil ))
  :initvals '(nil 50 nil)
  :icon 137
  :doc  "organise multi-seq  : chaque canal est mis sur une piste différente
La structure d'accords n'est pas gardée telle qu'elle etait dans l'objet d'origine, mais
reconstruite en fonction de la valeur de <align> (ms). Si align = nil, les accords ne
sont pas reconstruits .
Les pistes sont rangées dans l'ordre des canaux, sauf si on donne une liste ordonnées de canaux
(optional : canaux)"
(let* ((obj (pc+ obj)) 
       (lnotes (flat-once (seq->notes obj)))
       (canaux (if (null canaux) (canaux obj) canaux))
      lchseq)
 (dolist (chan canaux)
   (push (mkpiste1 lnotes chan 'remove) lchseq))
  (cp+ (align-chords (mki 'multi-seq :chord-seqs  (nreverse (remove nil lchseq)) ) align))))



(om::defmethod! channel->voice ((obj chord-seq) (align t) &optional (canaux nil ))
(let* ((obj (pc+ obj))  
       (lnotes (seq->notes obj))
        (canaux (if (null canaux) (canaux obj) canaux))
        lchseq)
  (dolist (chan canaux)
   (push (mkpiste1 lnotes chan 'remove) lchseq) )
  (cp+ (align-chords (mki 'multi-seq :chord-seqs  (nreverse (remove nil lchseq)) ) align))))


#| 

(om::defmethod! channel->voice ((obj chord-seq) (align t) &optional (canaux nil ))
(let ((lnotes (seq->notes obj))
      (canaux (if (null canaux) (canaux obj) canaux))
      lchseq)
  (dolist (chan canaux)
   (push (mkpiste1 lnotes chan 'remove) lchseq) )
  (align-chords (mki 'multi-seq :chord-seqs  (nreverse (remove nil lchseq)) ) align)))
|#


; note =  pitch (+ onset offset) dur vel chan


; retester seq-part (mkpiste changé pour éviter chseq vides)


(defun mkpiste1 (lnotes chan mode)
  (let ( res)
    (dolist (note lnotes)
      (if (= chan (fifth note))
        (push  note res)))
    (if (null res)
      (if (eq mode 'remove) () (notes->seq '((6000 0 10 0 1 999))))  ; note bidon pour éviter chseq vide
      (notes->seq (nreverse res)))))



(defun mkpiste2 (lnotes chan mode)
  (let (res)
    (dolist (note lnotes)
      (cond  ((= (first chan) (fifth note)) (push note res))
             ((= (second chan) (fifth note)) 
              (push (list (+ (first note) 50) (second note) (third note) (fourth note) (first chan) (sixth note))
                    res))))
    (if (null res)
      (if (eq mode 'remove) () (notes->seq '((6000 0 10 0 1 999))))
      (notes->seq (nreverse res)))))





(om::defmethod! seq-part ((object chord-seq)  (appar list) &optional (mode 'remove)
                              (delta 0))
  :initvals '(nil  ((1 3) (2 4) 6 8) 'remove 0)
  :menuins '((2 (("remove" 'remove) ("keep" 'keep))))
  :icon 137
  :doc  "lit ch-seq provenant d'un enregistrement ; rend liste de ch-seq pour multi-seq.
Les structures d'accord ne sont pas gardées; les ch-seq ne contiennent qu'une seule note. 
Les notes sont réparties dans différents ch-seq, en fct des canaux et de la liste <appar>
les canaux appariés sont fondus, le deuxiéme canal de chaque couple 
étant considéré comme jouant 1/4 de ton  -> chseq dont canal = 1er canal
si une seule piste indiquée : chseq gardant canal original
  syntaxe de <appar> : ((1 3) (2 4) 6 8)
optional : 'remove' = enlever les ch-seq vides (pas de notes correspondant au canal midi demandé)
'keep' = il y aura des ch-seq vides (permet de garder la même disposition entre plusieurs multi-seq
delta : permet de regrouper les notes en accord quand elle sont séparées de moins de ms
que la valeur indiquée (cf 'align-chords' ) "
(let ((lnotes (seq->notes object))
      lchseq)
  (dolist (chan appar)
    (if (atom chan) 
      (push (mkpiste1 lnotes chan mode) lchseq)
      (push (mkpiste2 lnotes chan mode) lchseq)))
  (if (> delta 0)
    (car-mapcar 'align-chords (nreverse (remove nil lchseq)) delta)
    (nreverse (remove nil lchseq)))))



     


(om::defmethod! chseq->mseq ((object chord-seq)  (chan list) &optional (mode 'remove)
                              )
  :initvals '(nil  (1 3 5 6 8) 'remove )
  :menuins '((2 (("remove" 'remove) ("keep" 'keep))))
  :icon 137
  :doc  "lit ch-seq  ; rend liste de ch-seq pour multi-seq.
Les structures d'accord ne sont pas gardées; les ch-seq ne contiennent qu'une seule note. 
Les notes sont réparties dans différents ch-seq, en fct des canaux 
optional : 'remove' = enlever les ch-seq vides (pas de notes correspondant au canal midi demandé)
'keep' = il y aura des ch-seq vides (permet de garder la même disposition entre plusieurs multi-seq
 "
(let ((lnotes (seq->notes object)) lchseq )
  (dolist (c chan)
    (push (mkpiste1 lnotes c mode) lchseq))
(mki 'multi-seq :chord-seqs
  (nreverse (remove nil lchseq)))))



(om::defmethod! velo ((object container)  (mulvel number) )
  :initvals '(nil  .5 )
  :icon 136
  :doc  "multiplie vélocités"
(ch-modif object 'om* mulvel 'lvel))

(om::defmethod! velo ((liste list)  (mulvel number) )
  :initvals '(nil .5)
  :icon 136
  :doc  "multiplie vélocités"
(om-round (om* liste mulvel)))


; ajouter choix de pistes pour multi-seq
(om::defmethod! tm-cresc ((object chord-seq)   (fact1st number) (factlast number)
                           &optional  (ref nil) (factref 2 ))
  :initvals '(nil 1.0 4.0 nil 3.0 )
   :icon 137
  :doc  "multiplie vélocités en forme de cresc ou de dim; multi-seq : ne marche bien que si les pistes contiennent 
des données reparties dans le temps de manière semblable"

(ch-modif object '= (om-round (l*curb  (lvel object) fact1st factlast  ref factref )) 'lvel))



(om::defmethod! tm-cresc ((object multi-seq)   (fact1st number) (factlast number)
                           &optional  (ref nil) (factref 2 ))
  (mki 'multi-seq :chord-seqs  (car-mapcar 'tm-cresc (chord-seqs object) fact1st factlast ref factref)))







(om::defmethod! duree ((object container)  (dur number))
  :initvals '(nil  2000 )
  :icon 136
  :doc  "donne à l'objet la durée dur, en faisant un scaling"
  
  (om* object (/ dur (obj-dur  object))))

;===Rename duree as tm-dur as to avoid the same name present in om2csound 28-06-2007===========

(om::defmethod! tm-dur ((object container)  (dur number) )
  :initvals '(nil  2000 )
  :icon 136
  :doc  "Returns the <object> with a a desired duration <dur>. The processing is done through scaling of the duration values."
  
  (duree object dur))
;==============================================================================================



(om::defmethod! canal ((object container)  (can t) )
  :initvals '(nil  1 )
  :icon 136
  :doc  "donne à l'objet le canal can"
  (ch-modif object '= can 'lchan))


(om::defmethod! newport ((object container)  (port t) )
  :initvals '(nil  1 )
  :icon 136
  :doc  "donne à l'objet le port <port>"
  (ch-modif object '= port 'lport))



(om::defmethod! arpeggio ((obj chord) (dur t) (direction t) (curve number) &optional (format 'chord)  )
  :initvals '(nil 1000 'up 50 'chord)
  :menuins '((2 (("up" 'up) ("down" 'down))) (4 (("chord" 'chord) ("chord-seq" 'chord-seq)))  )
   :icon 137
   :doc "arpeggiates chord . Order of chord is kept  (use <sort-chord> before <arpeggio> to order chord if necessary)
Duration of notes are not changed . Use <synch-fin> after <arpeggio> to synchronise endings.
Curve = 50  :linear  0-49 : rall   51-100  : accel"
   (let*  ((arp (om-round (deformer%  (n-arithm 0 dur (ch-length obj))  curve)))
           (offs (if (eq direction 'up) arp (reverse arp))))
        
 (if (eq format 'chord) (ch-modif obj '= offs 'loffset) 
     (explosion (ch-modif obj '= offs 'loffset)))))


(om::defmethod! accel-ral ((obj chord-seq)  (curve number) &optional (muldur 1.0)  )
   :initvals '(nil  50 1.0 )
   :icon 137
   :doc "creates accel or rall according to the value of <curve> :
Curve = 50  :linear  0-49 : rall   51-100  : accel
Only onsets are modified. See s-curve (also called seq-stretch-curve) for a more
sophisticated function.
multi-seq: will work well only if chord-seqs have same length
optional : muldur = multiplicator of total duration"
   (om*  (lonset-modif obj '= (om-round (deformer% (lonset obj) curve))) muldur))


(om::defmethod! accel-ral ((obj multi-seq)  (curve number) &optional (muldur 1.0)   )
   (mki 'multi-seq :chord-seqs  (car-mapcar 'accel-ral (chord-seqs obj) curve muldur)))




(om::defmethod! synch-fin ((object chord)  (end number) )
  :initvals '(nil  2000 )
  :icon 136
  :doc  "toutes les notes de l'objet finissent en même temps (utile si attaques décalées, mais fin synchrone)"
  (let ((nvdur 
    (loop for d in (ldur object)
          for o in (loffset object)
          collect (if (< end o) d (- end o)))))
  (ch-modif object '=    nvdur  'ldur)))

(om::defmethod! synch-fin ((object chord-seq)  (end number) )
  (let* ((lendpoints (om- end  (lonset object))))
         (mki 'chord-seq 
              :lmidic (loop for ch in (chords object)
                            for e in lendpoints
                            collect (synch-fin ch e))
              :lonset (lonset object))))


(om::defmethod! synch-fin ((object multi-seq)  (end number) )
  (mki 'multi-seq :chord-seqs  (car-mapcar 'synch-fin (chord-seqs object) end)))





(om::defmethod! ch-trim ((object chord)  (end number) )
  :initvals '(nil  2000 )
  :icon 136
  :doc  "no note will end later than end value <end>"
  (let* ((endpoint (om+ (loffset object) (ldur object)))
         (nvdur (om- (loop for i in endpoint
                      collect (min i end))(loffset object) )))
    (if (test<=0 nvdur) (print "end value too small") 
                 (ch-modif object '=    nvdur 'ldur))))

    
(om::defmethod! ch-trim ((object chord-seq)  (end number) )
  (let* ((lendpoints (om- end  (lonset object))))
         (mki 'chord-seq 
              :lmidic (loop for ch in (chords object)
                            for e in lendpoints
                            collect (ch-trim ch e))
              :lonset (lonset object))))


(om::defmethod! ch-trim ((object multi-seq)  (end number) )
  (mki 'multi-seq :chord-seqs  (car-mapcar 'ch-trim (chord-seqs object) end)))



(defun test<=0 (list)
(let ((flag nil))
  (dolist ( v list)
    (if (<= v 0) (setf  flag t)))
flag))






; nv methode qui regroupe chord-stretch et seq-stretch (qui sont gardés pour compatibilité)
; synonyme : om* (méthodes ajoutées en tête de fichier)
; il y a une méthode stretch dans geste.lisp ; heureusement les arguments sont compatibles, et le type différent!

(om::defmethod! stretch ((object chord-seq)  (muldur number) )
  :initvals '(nil  2)
  :icon 136
  :doc  "multiplie (onset), dur et offset par muldur. Accepte multi-seq"
  (seq-stretch object muldur))

(om::defmethod! stretch ((object multi-seq)  (muldur t) )
  (seq-stretch object muldur))

(om::defmethod! stretch ((object chord)  (muldur t) )
  (chord-stretch object muldur))




(om::defmethod! seq-stretch ((object chord-seq)  (muldur number) )
  :initvals '(nil  2)
  :icon 136
  :doc  "multiplie onset, dur et offset par muldur. Accepte multi-seq"
(mki 'chord-seq
       :LMidic (lmidic object)
       :lonset (om-round (om* muldur (lonset object)))
       :Ldur (om-round (om* muldur (ldur object )))
       :LOffset (om-round (om* muldur (loffset object )))
       :Lchan (lchan object )
       :Lvel (lvel object )
       :Lport (lport object )
       :legato (legato object)))




(om::defmethod seq-stretch ((self chord-seq) (num list))
  (append-seq
         (loop for n in num
               collect (seq-stretch  self n )) nil))
 
    


(om::defmethod! seq-stretch ((object multi-seq)  (muldur number) )
  (mki 'multi-seq
     :chord-seqs (car-mapcar 'seq-stretch (chord-seqs object) muldur)))



(om::defmethod! chord-stretch ((object chord)  (muldur number) )
  :initvals '(nil  2)
  :icon 136
  :doc  "multiplie  dur et offset par muldur"
(mki 'chord
       :LMidic (lmidic object)
       :Ldur (om-round (om* muldur (ldur object )))
       :LOffset (om-round (om* muldur (loffset object )))
       :Lchan (lchan object )
       :Lvel (lvel object )
       :Lport (lport object )
       ))


(om::defmethod! chord-stretch ((self chord) (num list))
  (let ((lchords 
         (loop for n in num
               collect (chord-stretch  self n ))))
    (mki 'chord-seq
         :lmidic lchords
         :lonset (om-round (dx->x 0 (om* (obj-dur self) num))))))



(om::defmethod! stretch-chunk ((object chord-seq) (begin number) (end number) (muldur number) &optional (voice 0 ))
  :initvals '(nil  1000 2000 0.7 0)
  :icon 137
  :doc  "stretches the region between begin and end by a factor muldur. Whatever is after <end> does not move.
Overlapping or gap may result. If object is a multi-seq , indicate voice(s) concerned"
  
  (paste-in-multi object (seq-stretch  (selectf object begin end  voice) muldur) voice begin 'replace))


(om::defmethod! stretch-chunk ((object multi-seq) (begin number) (end number) (muldur number) &optional (voice 0 ))
  :initvals '(nil  1000 2000 0.7 0)
  :icon 137
  :doc  "stretches the region between begin and end by a factor muldur. Whatever is after <end> does not move.
Overlapping or gap may result. If object is a multi-seq , indicate voice(s) concerned"
  
  (paste-in-multi object (seq-stretch  (selectf object begin end  voice) muldur) voice begin 'replace))




(om::defmethod! stretch-region ((object chord-seq) (begin number) (end number) (muldur number) )
  :initvals '(nil  1000 2000 0.7)
  :icon 136
  :doc  "stretches the region between begin and end by a factor muldur. 
The  region situated after <end> is moved accordingly. Accepts chord-seq and multi-seq "
  
  (let ((newdur (om-round(* muldur (- end begin)))))
    (chainer (list (selectf object 0 begin) (seq-stretch  (selectf object  begin end) muldur) (selectf object  end  nil))
             (list 0 begin (+ begin newdur)))))


(om::defmethod! stretch-region ((object multi-seq) (begin number) (end number) (muldur number) )
  
  (let ((newdur (om-round (* muldur (- end begin)))))
    
    (chainer (list (selectf object 0 begin) (seq-stretch  (selectf object  begin end) muldur) (selectf object  end  nil))
             (list 0 begin (+ begin newdur)))))





; à tester !!!   pb with offsets - pb if one elem in chseq
; ne marche pas bien avec mseq - il faudrait une vraie fct de transfert
; durées mal recalculées

(om::defmethod! seq-stretch-curve ((object chord-seq) 
                                   (fact1st number) (factlast number) 
                                   &optional (ref nil) (factref nil ))
  :initvals '(nil  1  2.5  nil nil)
  :icon 137
   :doc  "multiplies onsets, durs and offsets by Lcurb/2 or Lcurb/3 . Accepts multi-seq.
Use after align-chord  if division by 0 is detected."
  (let ((newonsets  (om-round (L*curb (lonset object) fact1st factlast ref factref))))
(mki 'chord-seq
       :LMidic (lmidic object)
       :lonset newonsets
       :Ldur (om-round (L*curb (ldur object) fact1st factlast ref factref))
       :LOffset (om-round (om* (om/ (x->dx newonsets) (x->dx (lonset object))) (loffset object )))
       :Lchan (lchan object )
       :Lvel (lvel object )
       :Lport (lport object )
       :legato (legato object))))


(om::defmethod! seq-stretch-curve ((object multi-seq)   
                                   (fact1st number) (factlast number) 
                                   &optional (ref nil) (factref nil ))
(mki 'multi-seq
     :chord-seqs 
     (loop for chseq in (chord-seqs object)
           collect (seq-stretch-curve chseq fact1st factlast ref factref))))



; nom simplifié
(om::defmethod! s-curve ((object multi-seq)   
                                   (fact1st number) (factlast number) 
                                   &optional (ref nil) (factref nil ))
:initvals '(nil  1  2.5  nil nil)
  :icon 137
  :doc  "multiplies onsets, durs and offsets by Lcurb/2 or Lcurb/3 . Accepts multi-seq.
Use after align-chord  if division by 0 is detected."
(seq-stretch-curve object fact1st factlast ref factref))


(om::defmethod! s-curve ((object chord-seq)   
                                   (fact1st number) (factlast number) 
                                   &optional (ref nil) (factref nil ))
:initvals '(nil  1  2.5  nil nil)
  :icon 137
   :doc  "multiplies onsets, durs and offsets by Lcurb/2 or Lcurb/3 . Accepts multi-seq.
Use after align-chord  if division by 0 is detected."
(seq-stretch-curve object fact1st factlast ref factref))





; (pitch vel offset dur chan)

(om::defmethod! filterC4 ((object chord) )
 :initvals '(nil )
  :icon 136
  :doc " sert  à neutraliser les C4 indésirables en leur donnant une vélocité nulle et un port=999"
(notes->chord 
 (loop for n in (chord->notes object)
      collect (if (and (= (first n) 6000) (= (second n) 100) (= (third n) 0) 
                            (= (fourth n) 1000) (= (fifth n) 1))
        (list 6000 0 0 1 1 999) n ))))
      

(om::defmethod! filterC4 ((object chord-seq) )
  (mki 'chord-seq
       :LMidic (mapcar 'filterC4 (get-chords object))
       :lonset (lonset object)
       :legato (legato object)))



(om::defmethod! filterC4 ((object multi-seq) )
  (mki 'multi-seq   :chord-seqs   (mapcar 'filterC4 (chord-seqs object))))




(om::defmethod! chords->seq ((chord chord) &rest chords )
 :initvals '(nil )
  :icon 137
  :doc  "Makes a chord-seq from a list of chords. The onsets are calculated according to offsets and maximum durations for each chord, so neither gap nor overlapping occurs within the new chord sequence. Allows for as many inlets as desired."

(let ((lchords (flat (list chord chords))))
(mki 'chord-seq :lmidic lchords  :lonset  (dx->x 0 (mapcar 'obj-dur lchords)))))


(om::defmethod! chords->seq ((chord list) &rest chords )
(mki 'chord-seq :lmidic chord  :lonset  (dx->x 0 (mapcar 'obj-dur chord))))

;==test this
;tester encore ces  méthodes - faire un append-seq pour mseq acceptant des dim. diff.


(om::defmethod! append-seq ((seq1 chord-seq) (seq2 chord-seq) &optional (interval nil))
  :initvals '(nil nil nil)
  :icon 137
  :doc  "Puts a chord sequence <seq2> after another chord sequence <seq1> at a specified interval <interval> after the attack of the last note within <seq1>. 
This means the starting point of <seq2> occurs at t = (attack of the last note in <seq1>, taking into account the offset) + <interval>. 
If <interval> = nil, <seq2> starts right after the end of <seq1>, taking into account both duration and offset. 
Accepts chord sequences and multi sequences of the same dimensions.
<seq1> may be a list of chord sequence objects or multi sequence objects. 
<interval> may be one interval or a list of intervals."

(let* ((lchords1 (get-chords seq1))
         (lchords2 (get-chords seq2))
         (lonset1 (butlast (lonset seq1)))   ; lonset comporte une valeur finale du chseq, à éliminer
         (lonset2 (lonset seq2))
         (endseq (list-max (flat (om+ lonset1 (om+ (ldur seq1) (loffset seq1))))))
         (lastattack (list-max (flat (om+ lonset1  (loffset seq1)))))
         (delta  (if (null interval)  endseq (+ interval lastattack))))
    (make-instance 'chord-seq
      :Lmidic (flat (list lchords1 lchords2))
      :Lonset (flat (list lonset1 (om+ lonset2  delta) )))))
    

(om::defmethod! append-seq ((seq1 chord-seq) (seq2 chord) &optional (interval nil))
  (append-seq seq1 (mki 'chord-seq :lmidic (list seq2) :lonset '(0)) interval))


(om::defmethod! append-seq ((seq1 multi-seq) (seq2 multi-seq) &optional (interval nil))
  (let* ((lchseq1 (chord-seqs seq1))
         (lchseq2 (chord-seqs seq2))
         (llonset1 (mapcar 'butlast  (lonset seq1)))  ;lonset comporte une valeur finale du chseq, à éliminer
         (llonset2  (lonset seq2))
         (endseq (list-max (flat (om+ llonset1 (om+ (ldur seq1) (loffset seq1))))))
         (lastattack (list-max (flat (om+ llonset1  (loffset seq1)))))
         (delta  (if (null interval)  endseq (+ interval lastattack)))
         res)
    (loop for chseq1 in lchseq1
          for chseq2 in lchseq2
          for lonset1 in llonset1
          for lonset2 in llonset2
          do (push (make-instance 'chord-seq
                     :Lmidic (flat (list (get-chords chseq1)  (get-chords chseq2)))
                     :Lonset (flat (list lonset1 (om+ lonset2  delta) ))) res)
          finally (return (mki 'multi-seq
                               :chord-seqs (nreverse res))))))




(om::defmethod! append-seq ((seq1 list) (seq2 t) &optional (interval nil))
   
   (let* ((seq1  (remove nil seq1))
         (interval (if (atom interval) (create-list (1- (length seq1)) interval) interval))
          (res (append-seq (first seq1) (second seq1)  (first interval))))
     (for (s 2 1 (1- (length seq1)))
       (setq res (append-seq res   (nth s seq1) 
                             (if (one-elem  interval) interval (nth (- s 1) interval)))))
     res))

;................................................................................


(om::defmethod! chainer ((seqs list) (onsets list))
  :initvals '(nil '(0 1000))
  :icon 136
  :doc  "Creates a sequence which contains all the sequences of the list <seqs>, pasted at the onsets indicated in the onset list <onsets>. Accepts multi-seqs - even multi-seqs with different dimensions."

(if (typep (first seqs) 'chord-seq) (chainer-chseq seqs onsets) (chainer-mseq seqs onsets) ))


(defun chainer-chseq (seqs onsets)
(let* ((lchords (flat (mapcar 'get-chords seqs)))
         (lastonset  (+ (last-elem (lonset (last-elem seqs))) (last-elem onsets)))
         (newonsets (loop for seq in seqs
                          for att in onsets
                          collect (om+ att (butlast (lonset seq))))))
    (sort-chords (make-instance 'chord-seq
                   :Lmidic  lchords 
                   :Lonset (flat (x-append newonsets lastonset))))))



(defun chainer-mseq (seqs onsets)
  (let* ((lseqs (mapcar 'chord-seqs seqs))
         (nmax (list-max (mapcar 'length lseqs))) res)

(for (i 0 1 (1- nmax))
  (let* ((chseqs (mapcar #'(lambda (x) (nth i x)) lseqs))

        (lonsets   (loop for j in (arithm-ser  0  (length-1 seqs) 1)
                         collect (if (not (null (nth j chseqs)))  (nth j onsets)))   ))

    (push (chainer  (list! (remove nil chseqs))   (list! (remove nil lonsets))) res))  )

 (mki 'multi-seq :chord-seqs (reverse res))))



(om::defmethod! add-chseq ((seq chord-seq) (mseq multi-seq) &optional (onset 0))
  :initvals '(nil nil 0)
  :icon 137
  :doc  "Adds (a) chord sequence(s) into a multi sequence.
<seq> may be a chord sequence, a list of chord sequence objects or a multi sequence object.
<onset> may be a list of the same length as the number of <seq> to be added."

  (mki 'multi-seq :chord-seqs (x-append (chord-seqs mseq) (lonset-modif seq '+ onset))))


(om::defmethod! add-chseq ((seq list) (mseq multi-seq) &optional (onset nil))
  (let* ( (onset (if (atom onset) (create-list (1+ (length seq)) onset) onset)) res 
        (seq (if (null onset ) seq 
                 (loop for s in seq
                       for o in onset
                   do (push (lonset-modif s '+ o) res)
                   finally (return (reverse res))))))
    (mki 'multi-seq :chord-seqs (x-append (chord-seqs mseq) seq))))

(om::defmethod! add-chseq ((seq multi-seq)   (mseq multi-seq) &optional (onset nil))
  (add-chseq (chord-seqs seq) mseq onset))



;................................................................................





(defmethod mixe-obj ((obj1 chord) (obj2 chord))
(mki 'chord
         :LMidic (flat (list (lmidic obj1) (lmidic obj2)))
         :Ldur (flat (list (ldur obj1) (ldur obj2)))
         :LOffset (flat (list (loffset obj1) (loffset obj2)))
         :Lchan (flat (list (lchan obj1) (lchan obj2)))
         :Lvel (flat (list (lvel obj1) (lvel obj2)))
         :Lport (flat (list (lport obj1) (lport obj2)))))

(defmethod mixe-obj ((obj1 chord-seq) (obj2 chord-seq))
(mki 'chord-seq
       :lmidic (flat (list  (get-chords obj1) (get-chords obj2)))
       :lonset (flat (list (butlast (lonset obj1)) (butlast (lonset obj2))
                           (max (list-max (lonset obj1))  (list-max (lonset obj2)))))))


(om::defmethod! mixer ((obj1 chord) (obj2 chord)  &rest objs )
  :initvals '(nil nil nil)
  :icon 137
  :doc  "Mixes two objects (chord, chord sequence or multi sequence objects). <rest> permits the mixing of more than two objects. <obj1> may be a list. <obj2> may be empty.
If the <obj>s are chords, the chords will be mixed into one single chord.
If the <obj>s are chord sequence objects or multi sequence objects, the chords will be mixed into a sequence containing the original chords."
  
  (let ((res (mixe-obj obj1 obj2))
        (objs (flat objs)))
    (if (null (car objs)) res
        (for (c 0 1 (1- (length objs)))
          (setq res (mixe-obj  res  (nth c objs) ))))
    res))

(defun multi-mixer (obj1)
  (mixer obj1 nil nil))

(om::defmethod! mixer ((obj1 list) (obj2 t)  &rest objs )
  (if (typep (car obj1) 'multi-seq)
    (multi-mixer obj1)
  (let ((res (mixe-obj (first obj1) (second  obj1))))
    (for (c 2 1 (1- (length obj1)))
          (setq res (mixe-obj  res  (nth c obj1) )))
    res)))

; a tester

(om::defmethod! mixer ((obj1 chord-seq) (obj2 chord-seq)  &rest objs )
  (let ((res (mixe-obj obj1 obj2))
        (objs (flat objs)))
    (if (null (car objs)) res
        (for (c 0 1 (1- (length objs)))
          (setq res   (mixe-obj   res (nth c objs) ))))
    (sort-chords res)))

(om::defmethod! mixer ((obj1 chord-seq) (obj2 null)  &rest objs )
  (mixer (chords obj1) nil))

(om::defmethod! mixer ((obj1 multi-seq) (obj2 multi-seq)  &rest objs )
(mki 'multi-seq :chord-seqs
     (car-mapcar 'mixer (chord-seqs (flat (list obj1 
                                                obj2
                                                (if (not (null (car objs))) objs)))))))

(om::defmethod! mixer ((obj1 multi-seq) (obj2 null)  &rest objs )
  (mixer (chord-seqs obj1) nil))

;................................................................................

(om::defmethod! paste-object ((seq1 chord-seq) (obj chord-seq) (onset integer) &optional
                              (mode 'merge))
  :initvals '(nil nil 1000 'merge)
  :menuins '((3 (("merge" 'merge)  ("replace" 'replace))))
  :icon 137
  :doc  "Pastes an object into a chord sequence object at a specified <onset>.
Optional mode <merge> means that the new object will be merged with the old material within the specified zone. 
Optional mode <erase> will erase the old material within the specified zone where the new object is pasted.
The zone is defined by the onsets  of the chords (the offsets are not taken into account).
<obj> may be a chord or a chord sequence object.
<obj> and <onset> may be lists of the same dimensions (i.e. lengths)."

  (let* ((seq1 (if (eq mode 'merge) seq1
                   (erase-chords  seq1 onset  (+ onset (last-elem (butlast (lonset obj)))))))
         (lonset1 (lonset seq1))
         (lonset2 (om+ onset (lonset obj)))
         (lastonset (max (last-elem  lonset1) (last-elem lonset2)))
         (contenu (sort-table (list (append (get-chords seq1) (get-chords obj))
                                    (append (butlast lonset1) (butlast lonset2))) 1)))

    (make-instance 'chord-seq
      :Lmidic (first contenu)
      :Lonset (x-append (second contenu) lastonset))))

(om::defmethod! paste-object ((seq1 chord-seq) (obj chord)  (onset integer) &optional
                                   (mode 'merge))
   (let* ((seq1 (if (eq mode 'merge) seq1
                    (erase-chords   seq1 onset  onset 'keep)))  ;     onset (obj-dur obj)))))
          (lonset1 (lonset seq1))
          (lastonset (max (last-elem  lonset1) onset))
          (contenu (sort-table (list (x-append (get-chords seq1) obj)
                                     (x-append (butlast lonset1) onset)) 1)))
     (make-instance 'chord-seq
       :Lmidic (first contenu)
       :Lonset (x-append (second contenu) lastonset))))

(om::defmethod! paste-object ((seq1 multi-seq) (obj multi-seq)  (onset integer) &optional
                              (mode 'merge))
  (mki 'multi-seq :chord-seqs 
       (double-mapcar 'paste-object (chord-seqs seq1) (chord-seqs obj) onset mode)))

(om::defmethod! paste-object ((seq1 t) (obj list)  (onset list) &optional
                                   (mode 'merge))

(loop for i in obj  for j in onset
      do (setq seq1 (paste-object seq1 i j mode))
      finally (return seq1)))


; il se peut que toutes les combinaisons possibles ne soient pas prises en compte
; il faudrait créer les voix nécessaires si la m-seq ne possède pas les voix indiquées dans voice

(om::defmethod! paste-in-multi ((mseq multi-seq) (obj chord-seq) (voice integer) (onset integer)
                                &optional  (mode 'merge))
  :initvals '(nil nil 0 1000 'merge)
  :menuins '((4 (("merge" 'merge)  ("replace" 'replace))))
  :icon 137
  :doc  "Pastes an object <obj> into a multi sequence object <mseq> at an absolute <onset> and a specified <voice>.
N.B. If <voice>=0, this means the first voice of the multi sequence object.
Optional mode <merge> means that the new object and the old material at the specified onset will be merged. 
Optional mode <erase> will erase the old material at the specified onset where the new object is pasted.
<obj> may be a chord or chord sequence object.
<obj>, <voice> and <onset> may be lists with the same dimensions."


  (let* ((lchseq  (chord-seqs mseq)) res)
    (for (n 0 1 (1- (length lchseq) ))
      (if (= n voice)  (push   (paste-object  (nth  n lchseq)  obj onset mode) res)
         (push (nth n lchseq) res)))
    (mki 'multi-seq :chord-seqs (nreverse res))))


; pour raccourcir le nom

(om::defmethod! pim ((mseq multi-seq) (obj t) (voice t) (onset t)
                                &optional  (mode 'merge))
  :initvals '(nil nil 0 1000 'merge)
  :menuins '((4 (("merge" 'merge)  ("replace" 'replace))))
  :icon 137
  (paste-in-multi mseq obj voice onset mode))



(om::defmethod! paste-in-multi ((mseq multi-seq) (obj chord) (voice integer) (onset integer)
                                &optional  (mode 'merge))
  (let* ((lchseq  (chord-seqs mseq)) res)
    (for (n 0 1 (1- (length lchseq) ))
      (if (= n voice) (push   (paste-object  (nth  n lchseq)  obj onset mode) res)
          (push (nth n lchseq) res)))
    (mki 'multi-seq :chord-seqs (nreverse res))))


(om::defmethod! paste-in-multi ((mseq multi-seq) (obj multi-seq) (voice t) (onset integer)
                                &optional  (mode 'merge))
  (paste-object mseq obj onset mode))



(om::defmethod! paste-in-multi ((mseq multi-seq) (obj list) (voice integer) (onset list)
                                &optional  (mode 'merge))
  (let ((res mseq) (nbobj (1- (length obj ))))
(for (n 0 1 nbobj)
  (setq res (paste-in-multi res (nth  n obj)   voice (nth  n onset) mode)))
res))


(om::defmethod! paste-in-multi ((mseq multi-seq) (obj list) (voice list) (onset list)
                                &optional  (mode 'merge))
  (let ((res mseq) (nbobj (1- (length obj ))))
(for (n 0 1 nbobj)
  (setq res (paste-in-multi res (nth  n obj)  (nth  n  voice) (nth  n onset) mode)))
res))


(om::defmethod! paste-in-multi ((mseq multi-seq) (obj chord-seq) (voice list) (onset integer)
                                &optional  (mode 'merge))
  (let ((res mseq) (nbvoice (1- (length voice ))))
(loop for v in voice
      do (setq res (paste-in-multi res   obj  v  onset mode))
      finally (return res))))


(om::defmethod! paste-in-multi ((mseq multi-seq) (obj multi-seq) (voice list) (onset integer)
                                &optional  (mode 'merge))
  (let ((res mseq) (nbvoices (1- (length voice ))))
(for (n 0 1 nbvoices)
  (setq res (paste-in-multi res (nth  n (chord-seqs obj))  (nth  n voice)  onset mode)))
res))





(om::defmethod! modif-sel ((obj chord-seq) (fct t) (arg t) (slot t) (deb t) (fin t)
                           &optional (piste nil))
  :initvals '(nil '+ 100 'lmidic 0 nil nil)
  :menuins '((3 (("midic" 'lmidic) ("vel" 'lvel) ("dur" 'ldur) ("offset" 'loffset) ("chan" 'lchan))))
  :icon 137
  :doc  "modifie la sélection; si multi-seq : indiquer la ou les piste(s) ; si piste=nil
toutes les pistes sont modifiées.
Marche avec chord ; dans ce cas , deb et fin se réfèrent au numéro d'ordre des notes à modifier"
(paste-object  obj  (ch-modif (selectf obj deb fin) fct arg slot)   deb 'replace))


(om::defmethod! modif-sel ((obj multi-seq) (fct t) (arg t) (slot t) (deb t) (fin t)
                           &optional (piste nil))
(let ((modif (cond ((null piste) (ch-modif (selectf obj deb fin piste) fct arg slot))
                   ((atom piste) (ch-modif (selectf obj deb fin piste) fct arg slot))
                   ((listp piste)
                    (loop for p in piste
                          collect (ch-modif (selectf obj deb fin p) fct arg slot)))))
      (deb (cond ((null piste) deb)
                   ((atom piste) deb)
                   ((listp piste) (create-list (length piste)) ) )) )
(paste-in-multi   obj  modif  piste   deb 'replace)))


(om::defmethod! modif-sel ((obj chord) (fct t) (arg t) (slot t) (deb t) (fin t)
                           &optional (piste nil))

(let ((ch1 (if (> deb 0) (ch-filter obj  0 (1- deb) ) nil))
      (ch2 (if (< fin (-  (ch-length  obj) 2))  (ch-filter obj (1+ fin) (1- (ch-length  obj))) nil)))
(mixer  (remove nil (list ch1 (ch-modif (ch-filter obj deb fin) fct arg slot) ch2)) nil)))




(om::defmethod! insert-object ((seq1 chord-seq) (obj chord)  (onset integer) &optional
                               (endinsert nil))
  :initvals '(nil nil 1000 nil)
  :icon 137
  :doc
  "Inserts an object <obj> into a sequence object <seq1> at an absolute <onset>.
In case <obj> is a chord sequence object or a multi sequence object, the ending of the insert corresponds with the last onset of the <obj>.
In case <obj> is a chord, the ending of the insert corresponds to the duration of the <obj>.
<seq1> may be a chord sequence object or a multi sequence object.
<obj> may be chird, chord sequence object or multi sequence object.

Optionals: With <endinsert> a different absolute onset (i.e. end of insert) can be specified."

  (let* ((durins (if endinsert (- endinsert onset) (ch-dur obj)))
         (endins (+ onset durins))
         (newonsets (loop for n in (lonset seq1)
                          collect (if (< n onset) n (+ n durins)))))
    (sort-chords 
     (make-instance 'chord-seq
       :Lmidic (x-append (get-chords seq1) obj )
       :Lonset (x-append (butlast newonsets) onset  (max endins (last-elem newonsets)))))))
                

(om::defmethod! insert-object ((seq1 chord-seq) (obj chord-seq)  (onset integer) &optional
                                    (endinsert nil))
   (let* ((durins (if endinsert (- endinsert onset) (list-max (lonset obj))))
          (endins (+ onset durins))
          (newonsets (loop for n in (lonset seq1)
                          collect (if (< n onset) n (+ n durins)))))
(sort-chords 
     (make-instance 'chord-seq
       :Lmidic (x-append (get-chords seq1) (get-chords obj) )
       :Lonset (x-append (butlast newonsets) (om+ (butlast (lonset obj)) onset)
                         (max endins (last-elem newonsets)))))))
                

(om::defmethod! insert-object ((seq1 multi-seq) (obj multi-seq)  (onset integer) &optional
                                    (endinsert nil))
   (let ((endins (if endinsert endinsert  (+ onset (list-max (lonset obj))))))
     (mki 'multi-seq
          :chord-seqs
          (double-mapcar 'insert-object (chord-seqs seq1) (chord-seqs obj) onset endins))))





(om::defmethod! insert-silence ((seq1 chord-seq)  (onset integer) (dur integer))
  :initvals '(nil  1000 1000)
  :icon 136
  :doc  "Inserts a silence of duration <dur> into a sequence <seq1> at an absolute onset <onset>.
<seq1> may be a chord sequence object or a multi sequence object."

(lonset-modif seq1 '=
         (loop for n in (lonset seq1)
                          collect (if (< n onset) n (+ n dur)))))
  

(om::defmethod! insert-silence ((seq1 multi-seq)  (onset integer) (dur integer))
  (mki 'multi-seq 
       :chord-seqs 
       (car-mapcar 'insert-silence (chord-seqs seq1) onset dur)))





(om::defmethod! erase-chords ((seq chord-seq) (debut integer)  (fin integer)
                                   &optional (mode 'keep))
  :initvals '(nil 1000 2000 'keep)
  :menuins '((3 (("keep" 'keep) ("snip" 'snip))))
  :icon 137
  :doc  "Erases the chords with onsets that are located within indicated <debut> and <fin> (included).
Optional mode <keep> means that the erased chords are replaced by silence.
Optional mode <snip> means that the two remaining pieces of the sequence are pasted together."

  (let* ((lchords (get-chords seq)) 
         (lonset (lonset seq))
         (interv (if (eq mode 'keep) 0 (- fin debut)))
         accords attaques)

    (loop for ch in lchords
          for att in lonset
          do (if ( < att debut) (progn (push ch accords)
                                       (push att attaques)))
          do (if ( > att fin) (progn (push ch accords)
                                       (push (- att interv) attaques)))
          finally (return (mki 'chord-seq
                                 :lmidic (nreverse accords)
                                 :lonset (x-append (nreverse attaques) (last-elem lonset))
                                 :legato (legato seq))))))


(om::defmethod! erase-chords ((seq multi-seq) (debut integer)  (fin integer) &optional (mode 'keep))
  (mki 'multi-seq :chord-seqs (car-mapcar 'erase-chords (chord-seqs seq) debut fin mode)))


;................................................................................
; mixtur replaces mixture 01-11-2006
; the old method is kept for compatiblity with old patches


(om::defmethod! mixtur ((object chord) (interv t)  (%amp t) (delai t) (mode  t)
                           &optional   (canal nil) (port nil))
  :initvals '(nil 700  100  0 'interv nil nil)
  :menuins '((4 (("interv" 'interv) ("nharm" 'nharm))))
  :icon 137
  :doc
"Adds an echo and/or harmonizer effect. Effectively, it returns a mixture of every note and its double, either at a fixed interval <interv> (midic) or as a defined harmonic partial <nharm>. The choice between these two options is made at <mode>. At the input <%ampl> the amplitude of the mixture can be defined as a percentage of (relative to) the amplitude of the original note. At <delai> a time interval or delay can be specified between the attacks of the mixture and the original note. <delai> essentially changes the offset of the mixture. <interv>, <%ampl> and <delai> may be atoms or lists of the same lengths.

Optionals: The mixtures may be routed through voices <canal> or ports <port> that differ from those of the original sounds. <canal> or <port> may be atoms or lists of the same length as <%ampl> and <delai>
"  
(mixer
  (addmixtur object interv %amp delai mode canal port ) object))



(defun addmixtur (object  interv %amp delai mode canal port  )
  (if (atom interv) (simplemixtur object  interv %amp delai mode canal
port )
      (multiplemixtur object  interv %amp delai mode canal port )))



(defun simplemixtur (object  interv %amp delai mode canal port)
  (mki 'chord
       :LMidic   (if (eq mode 'interv ) (om+ (lmidic object) interv)
                   (list! (n-harm  (lmidic object) interv 'midic 'chord)))
       :Ldur   (ldur object )
       :LOffset  (om+ (loffset object) delai)
       :Lchan    (if (null canal) (lchan object )  (list! canal))
       :Lvel    (om// (om* (lvel object ) %amp)  100)
       ;    :Lport    (if (null port) (lport object ) (list! port)))) toujours ce pb avec Lport...
       :Lport    (if (null port) (lport object ) (create-list (ch-length object) port))))


(defun multiplemixtur (object  interv %amp delai mode canal port )
  (let* ((nbtrans (length interv))
        (%amp (if (atom  %amp) (create-list nbtrans %amp) %amp))
        (delai (if (atom  delai) (create-list nbtrans delai) delai))
        (canal (if (atom  canal) (create-list nbtrans canal) canal))
        (port (if (atom  port) (create-list nbtrans port) port)))
    (mixer  (loop for i in interv
                 for a in %amp
                 for d in delai
                 for c in canal
                 for p in port
                 collect (simplemixtur object  i a d mode c p )) nil)))


(om::defmethod! mixtur ((object chord-seq) (interv t)  (%amp t) (delai
t) (mode  t)
                           &optional   (canal nil) (port nil))
  (mki 'chord-seq 
       :lmidic (loop for ch in (chords object)
                     collect (mixtur ch interv %amp delai mode canal
port ))
       :lonset (lonset object)))


(om::defmethod! mixtur ((object multi-seq) (interv t)  (%amp t) (delai
t) (mode  t)
                           &optional   (canal nil) (port nil))
  (mki 'multi-seq 
         :chord-seqs (loop for cs in (chord-seqs object)
                           collect (mixtur cs interv %amp delai mode
canal port ))))


;====Rename mixtur as ch-mixture 24-06-2007=================================


(om::defmethod! ch-mixture ((object chord) (interv t)  (%amp t) (delay t) (mode  t)
                           &optional   (channel nil) (port nil))

   :initvals '(nil 700  100  0 'interv nil nil)
   :menuins '((4 (("interv" 'interv) ("nharm" 'nharm))))
   :icon 137
   :doc
   "Adds an echo and/or harmonizer effect to an object: a chord object, a chord-seq or multi-seq. Effectively, it returns a mixture of every note and its double, either at a fixed interval <interv> (midic) or as a defined harmonic partial <nharm>. The choice between these two options is made at <mode>. At the input <%ampl> the amplitude of the mixture can be defined as a percentage of (relative to) the amplitude of the original note. At <delay> a time interval or delay can be specified between the attacks of the mixture and the original note. <delai> essentially changes the offset of the mixture. <interv>, <%ampl> and <delay> may be atoms or lists of the same lengths.
Optionals: The mixtures may be routed through voices <channel> or ports <port> that differ from those of the original sounds. <channel> or <port> may be atoms or lists of the same length as <%ampl> and <delay>."

   (mixtur object interv %amp delay mode channel port))


(om::defmethod! ch-mixture ((object chord-seq) (interv t)  (%amp t) (delay t) (mode  t)
                           &optional   (channel nil) (port nil))

   (mixtur object interv %amp delay mode channel port))


(om::defmethod! ch-mixture ((object multi-seq) (interv t)  (%amp t) (delay t) (mode  t)
                           &optional   (channel nil) (port nil))

   (mixtur object interv %amp delay mode channel port))


;===========================================================================


(om::defmethod! mixture ((object chord) (interv number)  (%amp integer) (delai integer) (mode  t)
                           &optional   (canal nil) (port nil) (format  'ch-seq) )
  :initvals '(nil 700  100  0 'interv nil nil 'ch-seq )
  :menuins '((7 (("ch-seq" 'ch-seq) ("multi-seq" 'multi-seq))))
  :menuins '((4 (("interv" 'interv) ("nharm" 'nharm))))
  :icon 137
  :doc  "double chaque note à un intervalle <interv> (midic) ou en calculant  son harmonique <nharm> , selon 
choix fait dans <mode>. 
%amp = % amplitude 
delai = délai entre original et mixture (ms). Interv, %amp, delai, peuvent être des listes
NB : Peut créer des échos ou des effets d'harmoniseur.
Pour les ch-seq et m-seq, il peut être utile d'utiliser align-chords, lorsque delai = 0
ch-seq et m-seq : toutes les config n'ont pas été testées"   
(mixer 
  (mixture1 object interv %amp delai canal port mode) object))



; j'ai du mettre un (list! )  pour canal et port, pour une raison inconnue...  (pour quand on a chord-seq , liste d'intervalles)


(defun mixture1 (object  interv %amp delai canal port mode )

(mki 'chord
                   :LMidic   (if (eq mode 'interv ) (om+ (lmidic object) interv) (n-harm  (lmidic object) interv 'midic 'chord))
                   :Ldur   (ldur object )
                   :LOffset  (om+ (loffset object) delai)
                   :Lchan    (if (null canal) (lchan object )  (list! canal))
                   :Lvel    (om// (om* (lvel object ) %amp)  100)
                   :Lport    (if (null port) (lport object ) (list! port))))


(om::defmethod! mixture ((object chord) (interv t)  (%amp t) (delai t) (mode  t)
                           &optional   (canal nil) (port nil) (format  'ch-seq))
(let* ((iter (max (length (list! interv)) (length (list! %amp)) (length (list! delai))))
      (interv (if (one-elem interv) (create-list iter interv) (list! interv)))
      (%amp (if (one-elem %amp) (create-list iter %amp)(list!  %amp)))
      (delai (if (one-elem delai) (create-list iter delai) (list! delai)))
      (canal (if (one-elem canal) (create-list iter canal) (list! canal)))
      (port (if (one-elem port) (create-list iter port) (list! port))))

(mixer (x-append object (loop for i in interv
                                for a in %amp
                                for d in delai
                                for c in canal
                                for p in port
                                collect (mixture1 object i a d c p mode ))) nil)))



(om::defmethod! mixture ((object chord-seq) (interv number)  (%amp integer) (delai integer) (mode  t)
                           &optional   (canal nil) (port nil) (format  'ch-seq))

(if (equal format 'ch-seq) (mixer (mixture-seq object interv %amp delai canal port) object)
   (mki 'multi-seq 
         :chord-seqs (list  object (mixture-seq object interv %amp delai canal port)))))




;  a revoir  (pb avec lport)


(defun mixture-seq (object  interv %amp delai canal port )
  
  (mki 'chord-seq
              :LMidic (if (eq mode 'interv ) (om+ (lmidic object) interv) (n-harm  (lmidic object) interv 'midic 'chord))
              :lonset (om+ (lonset object) delai)
              :Ldur (ldur object )
              :LOffset (loffset object )
              :Lchan (if (null canal) (lchan object ) canal)
              :Lvel (om// (om* (lvel object ) %amp) 100)
              :Lport (if (null port) (lport object ) port)))




;  a revoir



(om::defmethod! mixture ((object chord-seq) (interv t)  (%amp t) (delai t) (mode  t)
                           &optional   (canal nil) (port nil) (format  'ch-seq))
(print format)

(if (equal format 'ch-seq)
  (mki 'chord-seq
       :LMidic (loop for ch in (chords object)
                     collect (mixture ch interv %amp delai mode canal port format ))
       :lonset (lonset object))

  (mixt-mseq object interv %amp delai canal port )))
  


;  a revoir

(defun mixt-mseq (object interv %amp delai canal port )

(let* ((iter (max (length (list! interv)) (length (list! %amp)) (length (list! delai))))
      (interv     (if (one-elem interv) (create-list iter interv) (list! interv)))     
      (%amp      (if (one-elem %amp) (create-list iter %amp)(list!  %amp)))      
      (delai   (if (one-elem delai) (create-list iter delai) (list! delai)))   
      (canal    (if (one-elem canal) (create-list iter canal) (list! canal)))  
      (port  (print  (if (one-elem port) (create-list iter port) (list! port)))   )
      
      (res (loop for i in interv
                   for a in %amp
                   for d in delai
                   for c in canal
                   for p in port
                   collect (mixture object i a d  mode c p 'ch-seq))))

   (mki 'multi-seq 
         :chord-seqs (x-append object res))))




(om::defmethod! mixture ((object multi-seq) (interv number)  (%amp integer) (delai integer) (mode t)
                           &optional   (canal nil) (port nil) (format  'ch-seq))


(mki 'multi-seq  :chord-seqs
     (loop for ch in (chord-seqs object)
           collect (mixture ch interv %amp delai mode canal port 'ch-seq))))









(om::defmethod! diamanter ((chseq chord-seq) (nth-harm t)  (pcent% number) (dur integer)
                           (chan integer)  (port integer)
                           &optional (approx 4) (filter 10800))
  :initvals '(nil '(3 5) 50 1000 1 0 4 10800)
  :icon 137
  :doc
"Returns the harmonics <nth-harm> of every note of specified chords. It returns only a certain percentage of these harmonics which are aleatorically chosen. The harmonic attacks are spread out between the attack of the original chord and the attack of the next. Diamanter is a term coined by Olivier Messiaen. As an example, Messiaen explained in his orchestration class how the high register of the piano may add brilliance to orchestral sound textures: le piano diamante l’orchestre."
  
  (let* ((lchords (get-chords chseq))
         (lonset (lonset chseq))
         (linterv (x->dx lonset))
         accords attaques)
    
    (loop for ch in lchords
          for attack in lonset
          for interv in linterv
          with midics
          do (setq midics 
                   (permut-random (list-alea-filter 
                                   (list-filter #'(lambda (x)  (funcall '<= x filter))
                                                (flat (nth-polysp (lmidic ch) nth-harm 0 0 approx)) 'pass) 
                                   pcent%)))
         
          do (if  (eq midics nil) ()
                  (push 
                   (mki 'chord
                        :Lmidic midics
                        :LVel (list (round (om-mean (lvel ch))))
                        :LDur (list! dur)
                        :LOffset  (om-round (first-n 
                                             (n-arithm  0 interv (1+ (length midics)) 'inclus ) 
                                             (length midics)))
                        :LChan (list! chan)
                        :Lport (list! port))
                   accords))
           
          do (if (eq midics nil) ()
                 (push  attack attaques))
          
          finally (return (if (not (null accords))
                            (mki 'chord-seq
                                 :lmidic (nreverse accords)
                                 :lonset (x-append (nreverse attaques) (+ (first attaques) dur))
                                 :legato 0))))))
  
; -------------------------------------------------------------------------------------------------

; ----------------------------------engendrement--------------------------------------------


(om::defmethod! trill ((chord chord) (freq number) (dur number) )
                           
  :initvals '(nil  10 1000 )
  :icon 136
  :doc  "Iterates the  notes in <chord> with <freq> in hz and <dur> in msec. The velocity, channel and port remain identical to 1st note of the <chord>. If the <chord> has 2 notes, a trill is obtained, if the <chord> has only one note, the result is a repeated note. With 3 notes or more, various types of iterations can be created."

(let* ((nbnotes (round (* freq (/ dur 1000)))) (lgtrille (length (lmidic chord))) 
      (lnotes  (list-pos (flat (create-list  (ceiling (/ nbnotes lgtrille)) (lmidic chord))) 0 (1- nbnotes)))
      (loffsets  (om-round (x-arithm  0  (/ 1000 freq) nbnotes))))

  (ch-modif   
   (mki 'chord
        :lmidic lnotes
        :LVel (list (first (lvel chord)))
        :LDur (list! (round (/ 1000 freq)))
        :LOffset  loffsets
        :LChan (list! (first (lchan chord)))
        )   '=   (first(lport chord)) 'lport)))

; ----------  maquettes  ------------------

(om::defmethod! order-maq ( (maq ommaquette))
  :initvals '(nil)
  :icon 136
  :doc "donne la liste des objets de la maquette, dans l'ordre des offsets"
  (let ((objets (temporalboxes maq)))
         (second (sort-table (list (mapcar 'offset objets) objets) 0))))





#|   (om::defmethod! sel-maq ((self ommaquette) (offset t)  &optional (end nil))
  :initvals '(0 nil)
  :icon 137
  :doc "extrait le ou les objet(s) d'offset <offset>  . Optional : extrait touts les objets dont l'offset est compris entre
offset et end"

(if (eq end nil)
  (let ((objets (temporalboxes self)) res)
    (loop for ob in objets
      do (if (= (offset ob ) offset ) (push (value ob) res))
      finally (return (carlist! (reverse res)))))

   (selectf self offset end nil)))
|#


(defmethod test-editeur (self)
  (or (typep  self 'chord) (typep self 'note) (typep self 'chord-seq) (typep self 'multi-seq)))




(om::defmethod! sel-maq ((self ommaquette) (offset t)  &optional (end nil))
  :initvals '(nil 0 nil)
  :icon 137
  :numouts 2
  :doc "extrait le ou les objet(s) d'offset <offset>. Ne considère que les notes, chords, ch-seq, multi-seq.
   Optional : extrait touts les objets dont l'offset est compris entre
offset et end . output 1 = objets  output 2 = offsets"
  (if (eq end nil)
    (let ((objets (temporalboxes self)) res)
      (dolist ( ob objets )
        (if (and  (=  (offset ob ) offset )  (test-editeur (car (value ob)) ))
                  (push (car(value ob)) res)))
      (values (carlist! (reverse res)) offset))

    (let ((res  (sel-maq-interv self offset end )))
      (values (carlist! (first  res))  (carlist! (second res))))))





(defmethod sel-maq-interv (self start end )
  (let* ((objets (temporalboxes self))  (start (if (eq start nil) 0 start))
         (end (if (eq end nil) (l-max (mapcar 'offset objets)) end))
         res loffsets )
    (dolist  (ob objets)
      (if (and  (<> (offset ob ) start end '<>=) (test-editeur (car (value ob)) ))
        (progn (push (value ob) res) (push (offset ob ) loffsets))))
      (sort-table (list (flat res) loffsets ) 1)))



(om::defmethod! maq->mseq ((self ommaquette) (deb integer)  (fin integer) (delta  integer ))
  :initvals '(nil 0 10000 0)
  :icon 136
  :doc "le contenu de la maquette entre deb et fin est transféré dans un multi-seq. Les canaux et ports
sont repartis sur différentes pistes. Delta (ms) : permet de grouper les notes en accord (cf: align-chords)"

(let* ((liste-objets (first (multiple-value-list (sel-maq self deb fin))))   ; sel-maq filtre les boxes; ne garde que ch, ch-seq,mseq
      (liste-offsets (second (multiple-value-list (sel-maq self deb fin))))

      (objets 
       (loop for ob in liste-objets
             collect (cond ((typep ob 'multi-seq) (mixer ob nil))
                           ((typep ob 'chord) (mki 'chord-seq :lmidic (list ob)))
                           (t ob)))))
  (channel->voice (chainer objets (x-append 0 (om- (cdr liste-offsets) (first liste-offsets)))) delta)))
                                                            





;.................essais objets...............







(om::defmethod! midic->canal ((accord chord) (approx integer))
  :initvals '(nil 4)
  :icon 136
  :indoc '("accord, liste d'accords, ou seq" "approx")
  :doc  "rend accord approximé o˘ l'indication de 1/4 de ton ou de 1/8 ton est fournie
par le canal (+ 1 2 3 selon micro-int)"
  
  (let* ((hauteurs (approx-m (LMidic accord) approx))
         (canaux (LChan accord)))
    (make-instance 'chord
      :Lmidic (om* 100 (om-floor (om/ hauteurs 100)))
      :LVel (lvel accord)
      :LDur (ldur accord)
      :LOffset (loffset accord)
      :Lport (lport accord)
      :LChan (om+  canaux (om/ (om-modulo hauteurs 100)  25)))))


; liste d'accords
(om::defmethod! midic->canal ((accord list) (approx integer))
  (car-mapcar #'midic->canal  accord approx ))


; chord-seq
(om::defmethod! midic->canal ((seq chord-seq) (approx integer))
  (let* ((hauteurs (approx-m (LMidic seq) approx))
         (canaux (LChan seq)))
    (make-instance 'chord-seq
      :Lmidic (om* 100 (om-floor (om/ hauteurs 100)))
      :LVel (lvel seq)
      :LDur (ldur seq)
      :LOffset (loffset seq)
      :Lonset (lonset seq)
      :Lport (lport accord)
      :LChan (om+  canaux (om/ (om-modulo hauteurs 100)  25)))))





; faire aussi pour multi-seq




; =============================== files i/o ================================

(defun read-file-list  (fichier  ) 
  (let (list item   
             (nom (if (null fichier ) (om-choose-file-dialog ) fichier)))
    (when nom
      (with-open-file (file nom :direction :input 
                            :if-does-not-exist nil)
        (while (not (eq :eof (setq item (read file nil :eof))))
          (push item list))) 
       (nreverse list)
       )))

(defun read-file-array (dimension  fichier  )  
  (let (item   
        (nom (if (equal fichier "name") (om-choose-file-dialog ) fichier))
        (tab (make-array dimension :adjustable t  :fill-pointer 0)))
    (when nom
      (with-open-file (file nom :direction :input 
                            :if-does-not-exist nil)
        (while (not (eq :eof (setq item (read file nil :eof))))
          (vector-push item tab))))
    
    (setf tab (adjust-array tab (fill-pointer tab) :fill-pointer t :initial-element 0))
    
    (print (format nil "nb éléments: ~A " (fill-pointer tab)))
      tab))


(om::defmethod! read-file (&optional (fichier nil )  (format 'list )
                                        (dimension 100000 )) 
   :initvals '(nil 'list 100000)
   :menuins '((1 (("list" 'list) ("array" 'array)) ))
   :icon 136
   :doc "lit un fichier texte
ext: fichier = donner nom de fichier avec son ''path'' - sinon un dialogue s'ouvre
format de sortie : list par défaut, array par option "
   (if (equal format 'array) (read-file-array  dimension fichier) (read-file-list fichier)))
        


(om::defmethod! write-file ((list t) 
                               &optional  (sep 'rien ) (fichier nil ) (fmat nil))  
   :initvals '('(1 2 3 4 5) 'rien nil nil )
   :menuins '((1  (("rien" 'rien) ("espace" 'espace) ("ligne" 'ligne) ("tab" 'tab))))
   :icon 137
   :doc " option : séparateur de données (rien, espace, ligne, tab)
''rien''  suppose que la liste est déjà formatée "
   (let ((nom (if (equal fichier 'nil) (om-choose-new-file-dialog ) fichier))
         (char (case sep
                 (rien  ""  )
                 (espace #\Space)
                 (ligne #\newline)
                 (tab #\tab))))
     (print sep)
     (print char)
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         (while list
                (if (null fmat ) (princ  (pop list) file)
                    (princ  (format  nil fmat (pop list)) file))
                (princ char file)))
       nil)))




(om::defmethod! ll-write-file ((list list) 
                                  &optional  (sep 'espace )  (fichier nil ) (fmat 0 ))
   
   :initvals '('((1 2 3) (a b c)) 'espace nil 0)
   :menuins '((1  ( ("espace" 'espace)  ("tab" 'tab))))
   :icon 137
   :doc " écrit sur fichier une liste de listes : chaque sous-liste sera écrite comme 
une nouvelle ligne
options : séparateur de données à l'intérieur de chaque ligne  ( space ou tab)
          nom de fichier
          fmat =  nombre de chiffres pour représenter les nombres (évite aussi notation
exponentielle) . Si fmat=0 les nombres sont écrits tels quels  "
   
   (let ((nom (if (equal fichier 'nil) (om-choose-new-file-dialog ) fichier))
         (char (case sep
                 (espace #\Space)
                 (tab #\tab))))
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         (dolist (sousliste list)
           (dolist (i sousliste) 
             (if (= fmat 0) 
               (princ i file)
               (if (numberp i) (format file (format nil "~A~AF"  '~ fmat) i) (princ  i file)))
             (princ char file))
           (princ #\newline  file)))
       nil))
   (print  "fichier  écrit   ")
   nil)


; ======================  opérations sur tableaux  ======================


(defvar *tab*)
(defvar *fcttab*) 
(defvar *valtab*)
(defvar *tlim*)

(defun dimensions (tableau)
  (let ((ndim (array-rank tableau)))
    (mapcar #'(lambda (x) (array-dimension tableau x)) (arithm-ser 0 1 (1- ndim)))))



(om::defmethod! creer-tab ((dimensions t) (valinit t))
   :initvals '('(5) '(1 2 3 4 5))
   :indoc '( "dimensions" "valinit")
   :icon 136
   :doc "crée un tableau - dimensions = liste des dimensions
valinit : si c'est un atome, toutes les cases du tableau
sont initialisées à cette valeur
sinon entrer  une liste de structure identique à celle 
du tableau"
   
   
   (if (atom valinit) (t-init (make-array dimensions ) valinit)
       (make-array dimensions  :initial-contents valinit)))




(defun recurindexinit (tindex i vali maxi  )
  (if (< i 0) ()
      (cond ((and (= i maxi) (= (nth i tindex ) (nth i *tlim* ))) (setf (nth i tindex ) 0) 
                   (setq i (1- i))  (recurindexinit  tindex i i maxi  ))
            ((> (nth i tindex ) (nth i *tlim* )) (setf (nth i tindex ) 0) (setq i (1- i)) 
                     (recurindexinit  tindex  i i maxi  ))
            (t (setq i vali) (setf (nth i tindex ) (1+ (nth i tindex ))) 
               (if (<=  (nth i tindex ) (nth i *tlim* )) 
                 ;(print tindex))
                 (setf (apply #'aref *tab* tindex) 
                       (setf (apply #'aref *tab* tindex) *valtab* )))
               (recurindexinit  tindex i maxi maxi  ))
            )))

(om::defmethod! t-init ((*tab* t)  (*valtab* t))
   :initvals '(t t)
   :indoc '( "tab" "valtab")
   :icon 136
   :doc ""
   
   (let* ((*tlim*  (om- (dimensions *tab*) 1)) (rank (length *tlim*)) 
          (maxi (1- rank)) 
          (tindex  (create-list  rank 0)))
     (setf (apply #'aref *tab* tindex) 
           (setf (apply #'aref *tab* tindex) *valtab* ))
     (recurindexinit  tindex maxi maxi maxi  )
     *tab*))


(om::defmethod!  l-aref ((tableau t) (indices t))
   :initvals '('(5) '(1 2 3 4 5))
   :indoc '( "tableau" "indices")
   :icon 136
   :doc "donne les cases d'un tableau spécifiées par <indices>
la structure de <indices> doit refléter celle du tableau
ex: pour un tableau à deux dim :
'((0 0) (1 0))  ou même : '((0 0))"
   
   
   (cond  ((atom indices) (aref  tableau  indices ) )
          ((= (array-rank tableau) 1) (mapcar #'(lambda  (x) (aref  tableau  x)) indices))
          (t (mapcar #'(lambda  (x) (apply #'aref  tableau  x )) indices  ))))


(om::defmethod! t-substit ((tab t) (case list) (val list))
   :initvals '(t '(1 2) '(1 2))
   :indoc '( "tableau" "case" "val")
   :icon 136
   :doc ""
   (let ((case (list! case)))
     (dolist  (l case)
       (setf (aref tab l) val))
     tab))



(defun recurindex ( tindex i vali maxi  )
  (if (< i 0) ()
      (cond ((and (= i maxi) (= (nth i tindex ) (nth i *tlim* ))) (setf (nth i tindex ) 0) 
                   (setq i (1- i))  (recurindex  tindex i i maxi  ))
            ((> (nth i tindex ) (nth i *tlim* )) (setf (nth i tindex ) 0) (setq i (1- i)) 
                     (recurindex  tindex  i i maxi  ))
            (t (setq i vali) (setf (nth i tindex ) (1+ (nth i tindex ))) 
               (if (<=  (nth i tindex ) (nth i *tlim* )) 
                 ;(print tindex))
                 (setf (apply #'aref *tab* tindex) 
                       (funcall *fcttab* (apply #'aref *tab* tindex) *valtab* )))
               (recurindex   tindex i maxi maxi  ))
            )))

(om::defmethod! t-oper ((tableau t) (*fcttab* t) (*valtab* t))
   :initvals '(t '+ t)
   :indoc '( "tableau" "fcttab" "valtab")
   :icon 136
   :doc "t-oper copie le tableau d'entrée, mais
attention au sort futur du tableau de sortie!"
   
   (let* (( *tab* (make-array (dimensions tableau) :initial-contents (coerce tableau 'list)))
          (*tlim*  (g- (dimensions tableau) 1)) (rank (length *tlim*)) (maxi (1- rank)) 
          (tindex  (create-list  rank 0)))
     (setf (apply #'aref *tab* tindex) 
           (funcall *fcttab* (apply #'aref *tab* tindex) *valtab* ))
     (recurindex  tindex maxi maxi maxi  )
     *tab*))




(om::defmethod! v-oper-1 ((vecteur t) (fct t) (val t))
   :initvals '(t '+ t)
   :indoc '( "vecteur" "fct" "val")
   :icon 136
   :doc " opération sur vecteur 
v-oper copie le vecteur d'entrée, mais
attention au sort futur du vecteur de sortie!"
   
   (let* ((long  (length vecteur) )
          ( tab (make-array  long  :initial-contents (coerce vecteur 'list))))
     (dotimes (n long)
       (setf (aref tab n) 
             (funcall fct (aref tab n) val )))
     tab))


(om::defmethod! v-oper ((vecteurs t) (fct t ) (val t))
   :initvals '(t '+ t)
   :indoc '( "vecteur" "fct" "val")
   :icon 136
   :doc " opération sur vecteur 
v-oper copie le vecteur d'entrée, mais
attention au sort futur du vecteur de sortie!
accepte listes de vecteurs"
   (car-mapcar  'v-oper-1  vecteurs fct val))


; ceci ne fonctionne plus avec Lisp 4.0

(om::defmethod! l-coerce ((lliste list) (format symbol))
   :initvals '('(1 2) 'list)
   :indoc '( "lliste" "format")
   :menuins '((1  ( ("list" 'list)  ("array" 'array))))
   :icon 136
   :doc "lliste must be a list of list. This funct apparently transforms
each list  of list into a vector"
   (car-mapcar  'coerce lliste (if (equal format 'list) 'list 'array)))



(defun complete-liste1 (liste long elem)
  (x-append liste (create-list (-  long (length liste)) elem)))

(defun complete-lliste (liste long elem)
  (car-mapcar 'complete-liste1 liste long elem))


(om::defmethod! liste->tab ((liste list))
   :initvals '( '((1 2) (1 2)) )
   :indoc '( "lliste" )
   :icon 136
   :doc "transforme une liste de listes en tableau à deux dim
dim1 = nb des sous-listes
dim2 = longueur max des sous-listes"
   
   (let* ((larg (list-max (mapcar 'length liste)))
          (liste (complete-lliste liste larg nil))
          (dim (list (length liste) larg)))
     (make-array dim :initial-contents liste)))






;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; Fondamentales virtuelles multiples. Algorithme par Olivier Delerue.
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------






(defvar *tolerance* 1.0293 ) ;;; quart de ton
(defvar *frequence-min* 30 )

;;; (setf *frequence-min* 20 ) 
;;; (setf *tolerance* 1.01 )    un pour cent d'erreur

;;; -------------------------------------------------------------------------
;;; Définition des classes 
;;; -------------------------------------------------------------------------

(defclass spectre ( )
  ((fondamentales :initarg :fondamentales :initform () :accessor fondamentales )
   (partiels :initarg :partiels :initform ()  :type list  :accessor partiels :documentation "une liste d'objets partiels" )
   ))

(defclass partiel () ((frequence :initarg :frequence :initform 0 :accessor frequence )))
    
(defclass regroupement () ( (spectres :initarg :spectres :initform ()  :accessor spectres :type list )))

(defclass classement () 
  ((regroupements :initarg :regroupements :initform () :accessor regroupements :type list ) 
   (itere0-p :initform nil :accessor itere0-p ))
  )

;;; -------------------------------------------------------------------------
;;; Fonctions utiles  
;;; -------------------------------------------------------------------------


(defmethod fondamentales-communes-spectres ((sp1 spectre) (sp2 spectre))
  (fondamentales-communes (fondamentales sp1) (fondamentales sp2 ))
  )

(defmethod fondamentales-communes-spectres ((sp1 spectre) (sp2 list))
  (fondamentales-communes (fondamentales sp1) sp2)
  )

(defmethod fondamentales-communes ((liste1 list) (liste2 list) )
  (if (and liste1 liste2)
    (let ((temp (intersection-p (car liste1) (car liste2) )))
      (if temp
        (cons temp (fondamentales-communes (cdr liste1) (cdr liste2)) )
        (if (<= (caar liste1) (caar liste2) )
          (fondamentales-communes liste1 (cdr liste2) )
          (fondamentales-communes (cdr liste1) liste2 )
          )
        )
      )
    ()
    )
  )

(defmethod fondamentales-communes-liste ( liste-spectres )
  (if (cdr liste-spectres)
    (fondamentales-communes (fondamentales (car liste-spectres)) 
                            (fondamentales-communes-liste (cdr liste-spectres)))
    (fondamentales (car liste-spectres))
    )
  )

(defmethod premiere-fondamentale-commune ((sp1 spectre) (sp2 spectre))
  (premiere-fondamentale-commune (fondamentales sp1) (fondamentales sp2))
  )

(defmethod premiere-fondamentale-commune ((liste1 list) (liste2 list))
  (if (and liste1 liste2)
    (let ((temp (intersection-p (car liste1) (car liste2) )))
      (if temp
        temp
        (if (<= (caar liste1) (caar liste2) )
          (premiere-fondamentale-commune liste1 (cdr liste2) )
          (premiere-fondamentale-commune (cdr liste1) liste2 )
          )
        )
      )
    '(1 1)
    )
  )

(defun cree-partiels (liste-frequences) 
  (loop for item in liste-frequences collect (make-instance 'partiel :frequence item ) )
  )

(defun cree-liste-spectres (liste-frequences tolerance freq-min)
  (loop for item in liste-frequences collect 
        (make-instance 'spectre 
          :partiels (list (make-instance 'partiel :frequence item ) )
          :fondamentales (liste-fondamentales-possibles (list (/ item tolerance ) (* item tolerance )) freq-min))
        )
  )

(defun liste-fondamentales-possibles ( partiels freq-min)
  (loop for sub from 1
        while (>= (/ (car partiels) sub)  freq-min )
        collect (list (float (/ (car partiels) sub))  (float (/ (cadr partiels) sub ))    )
        )
  )
        
(defun intersection-p ( liste1 liste2 ) 
  (if (or (and (<= (car liste2) (cadr liste1) ) (>= (car liste2) (car liste1) ))  
          (and (<= (cadr liste2) (cadr liste1)) (>= (cadr liste2) (car liste1) )) 
          (and (<= (car liste2) (car liste1)) (>= (cadr liste2) (cadr liste1) ))
          )
    (list (max (car liste1) (car liste2) ) (min (cadr liste1) (cadr liste2) ) )
    ()
    )
  )

(defun make-classement (liste-partiels tolerance freq-min)
  (make-instance 'classement     
    :regroupements (list (make-instance 'regroupement 
                           :spectres (cree-liste-spectres liste-partiels  tolerance freq-min))) ))


(defmethod regroupe-deux-spectres ((spectre1 spectre) (spectre2 spectre))
  (make-instance 'spectre 
    :partiels (append (partiels spectre1 ) (partiels spectre2 )) 
    :fondamentales (fondamentales-communes-spectres spectre1 spectre2)
    )
  )


;;; -------------------------------------------------------------------------
;;; Fonction de distance. 
;;; -------------------------------------------------------------------------

(defmethod distance-nulle ((sp1 spectre) (sp2 spectre))
  (if (> (caar (fondamentales sp1)) (caar (fondamentales sp2)))
    (distance-nulle sp2 sp1)
    (let ((temp (premiere-fondamentale-commune (list (car (fondamentales sp1))) (fondamentales sp2)) ))     
      (if (eq (car temp) 1)
        ()
        temp
        )
      )
    )
  )

;;; -------------------------------------------------------------------------
;;; Methode globale d'iteration sur un classement 
;;; -------------------------------------------------------------------------

(defmethod iteration ((self classement))
  (if (itere0-p self )
    (if (> (length ( spectres (car (last ( regroupements self ))))) 1 )
      (let ((temp (regroupe-distance-non-nulle (car (last ( regroupements self )))) ))
        (if temp 
          (true (setf (regroupements self) 
                      (append ( regroupements self ) 
                              (list temp)) ))
          nil
          )
        )
      nil
      )
    (progn 
        (setf (regroupements self) 
              (append (regroupements self)
                      (list (regroupe-spectres-distance-nulle (car (last (regroupements self )))))))
        (setf (itere0-p self) T )
      )
    )
  )

;;; -------------------------------------------------------------------------
;;; Regroupement de spectres à distance nulle
;;; -------------------------------------------------------------------------

(defmethod regroupe-spectres-distance-nulle ((self regroupement))
  (make-instance 'regroupement 
    :spectres (bidon (spectres self))
    )
  )
  
(defun bidon (liste-spectres)
  (if liste-spectres
    (let ((new-spectre (make-instance 'spectre 
                         :partiels (partiels (car liste-spectres))
                         :fondamentales (fondamentales (car liste-spectres ))
                         )))
      (let ((temp 
             (loop for item in (cdr liste-spectres )
                   when (distance-nulle new-spectre item)
                   do (progn
                        (setf (partiels new-spectre) (append (partiels new-spectre) (partiels item) ) )
                        (setf (fondamentales new-spectre) (fondamentales-communes-spectres new-spectre item))
                        )
                  and collect item
                   )))
        (cons new-spectre (bidon (set-difference (cdr liste-spectres) temp )))
        )
      )
     
    ()
    )
  )

;;; -------------------------------------------------------------------------
;;; Regroupement de spectres à distance NON nulle
;;; -------------------------------------------------------------------------

(defmethod regroupe-distance-non-nulle ((self regroupement))
  (let ((temp (evalue-distances self )))
    (if (eq (caaar temp) 1)
      ()
     (make-instance 'regroupement 
        :spectres (append (list (regroupe-deux-spectres (cadar temp) (caddar temp) ) ) 
                          (set-difference ( spectres self) (cdar temp))
                          )
        )
      )
    )
  )

(defmethod evalue-distances (( self regroupement ) )
  (let (temp)
    (loop for x on ( spectres self) do
          (loop for y in (cdr x )
                do (push (list (premiere-fondamentale-commune (car x) y ) (car x) y ) temp )
                )
          )
    (sort temp #'> :key #'caar )
    )
  )





;===========================================================================
;;; INTERFACE
;===========================================================================


(defun join-fund-to-spec (fund spec) 
  (if (<= (first fund) (first spec) (second fund))
    spec
    (cons (second fund) spec)))




(defun gather-duplicates-1 (list accum)
  (cond ((null list) (reverse accum ))
        ((or (null (first accum)) (= (first list) (first (first accum))))
         (gather-duplicates-1 (rest list) (cons (cons (first list) (first
                                                                    accum)) (rest accum))))
        (t (gather-duplicates-1 (rest list) (cons (list (first list))
                                                  accum)))))


(defun gather-duplicates (list)
  (and list (gather-duplicates-1 list (list (list)))))






;======================================  INFOS  ===============================
;;(list-filter  #'(lambda (x)  (funcall '< x 5  )) '(1 2 3 10) 'reject)
;;(do-filter '(1 2 3 10) #'(lambda (x)  (funcall '< x 5  )))


;======================================  19 aug 2016 new additions to OMTristan from TM  ============




(om::defmethod! iso-dur ((object chord-seq) (dur number) (mode t))
  :initvals '(nil 200  'durtot)
  :icon 136
  :menuins (list (list 2 '(("interval" 'interv) ("durtotal" 'durtot) )))
  :doc  "place les notes d'un chord-seq ‡ intervalles rÈguliers; on donne l'intervalle de temps entre 2 notes,
 ou la durÈe totale. Les durÈes sont Ègales aux intervalles de durÈe; dans l'option 'interval' la derniËre 
durÈe est conservÈe comme dans le ch-seq original. Accepte multi-seq.
Faire prÈcÈder d'un align-chords si nÈcessaire"

  (let ((res (if (eq mode 'interv)
                 (mki 'chord-seq
                  :LMidic  (lmidic object)  
                  :Lchan  (lchan object)
                  :Lvel (lvel object)
                  :LOffset (loffset object)
                  :Ldur  (ldur object)
                 :lonset  (x-arithm 0 dur (length (ldur object))))
                  (mki 'chord-seq
                  :LMidic  (lmidic object)  
                  :Lchan  (lchan object)
                  :Lvel (lvel object)
                  :LOffset (loffset object)
                  :Ldur  (om-round (create-list (length (ldur object)) (om/ dur (length (ldur object)))))
                 :lonset  (om-round (dx->x 0 (create-list (length (ldur object)) (om/ dur (length (ldur object))))))))))

    (newport   (lier res) (lport object))))


(om::defmethod! iso-dur ((object multi-seq) (dur number) (mode t))
  :initvals '(nil 200  'durtot)
  :icon 136
  :menuins (list (list 2 '(("interval" 'interv) ("durtotal" 'durtot) )))

   (let* ((res ))
     (dolist (seq (chord-seqs object))
       (push (iso-dur seq dur mode) res    ))
     (mki 'multi-seq :chord-seqs (reverse res))))





(om::defmethod! 0start ((object t)  ) 
  :initvals '(nil )
  :icon 136
  (lonset-modif object  '- (list-min (lonset object))))



(om::defmethod! ch-onsets->seq ((chords list) (onsets list) &optional delta)
 :initvals '(nil nil nil)
  :icon  137
  :doc  "makes a chord-seq out of a list of chords and a list of corresponding onsets.If delta not null, 
chords will be grouped with the function <align-chords> . Same as 'chainer' , but adds the possibility
of chord alignment"
(if delta (align-chords (mki 'chord-seq :lmidic chords  :lonset  onsets) delta)
    (mki 'chord-seq :lmidic chords  :lonset  onsets)))





(om::defmethod! append-mseq ((liste list)  &optional (interval nil))
  :initvals '(nil nil)
  :icon 137
  :doc  "construit un multi-seq unique ‡ partir d'une liste de multi-seqs de taille diffÈrente; 
la liste peut comporter des chord-seqs "

(let 
    ((taille (list-max (loop for seq in liste
                             collect (if (typep seq 'multi-seq) (length (chord-seqs seq)) 1)))))

  (ch-test-filter  (append-seq
                             (loop for seq in liste
                                   do (print seq)
                                   collect (if (typep seq 'multi-seq) 
                                               (paste-in-multi  (multi-seq-vide taille) (chord-seqs seq) (arithm-ser 0 (length-1 (chord-seqs seq)) 1) (create-list  (length (chord-seqs seq)) 0) 'replace)
                                             (paste-in-multi (multi-seq-vide taille) seq 0 0 'replace))) nil interval) '= 0 'lvel)))



(om::defmethod! copy-paste ((seq multi-seq) (begin number)  (end number) (destination number)
                                   (orivoices t)  (destvoices t)   &optional (mode 'merge))
:initvals '(nil 1000 2000 4000 '(0 1) '(0 1) 'merge)
  :menuins (list (list 6 '(("merge" 'merge)  ("replace" 'replace))))
  :icon 137
  :doc  "If seq is a multi-seq, pastes at <destination> in voices <destvoices> the excerpt between 
<begin> and <end> in voices <orivoices>. If seq is a chord-seq, voices are not considered.
 "
  (paste-in-multi  seq (selectf  seq begin end orivoices) destvoices destination mode))



(om::defmethod! copy-paste ((seq chord-seq) (begin number)  (end number) (destination number)
                                   (orivoices t)  (destvoices t)   &optional (mode 'merge))
  (paste-object  seq (selectf  seq begin end)  destination mode))






(om::defmethod! cresc-gen ((obj chord-seq) (begin t) (end t) (curve number)  )
  :initvals '(nil 30 100 50)
   :icon 136
   :doc "creates cresc or dim from velocity value <begin> to velocity value <end>
Curve : if = 50 , linear progression   . If curve < 50 = more exponential . If curve > 50 = more logarithmic
Does not work on multi-seqs
If division by 0 error generated, slightly change one of the values"

   (let*  ((velo (om-round (deformer%  (n-arithm  begin end (length (lmidic obj)))  curve))))
           (ch-modif obj '= velo 'lvel) ))




(om::defmethod! map-channel ((obj chord) (mapping list))
  :initvals '(nil (((0 1) (1  3))))
  :indoc '("chord" "mapping list")
  :icon 136
  :doc  "gives new port and channel numbers . Format of mapping list :
(((oldport oldchannel) (newport newchannel)) ((oldport oldchannel) (newport newchannel))...)
ex :  ( ((0 1) (3 5))  ((1 3) (1 7))  ((1 9) (2 11)) )
if one change only, don't forget outer brackets : ( ((0 1) (3 5)) ) "
  
  (let ((ports (lport obj)) (chans (lchan obj))  )
    (for  (i 0 1 (length-1 ports))
      (loop for m in mapping
            
            do (if   (and (= (nth i ports) (car (car m))) (= (nth i chans) (second (car m))))
                 (progn (setf (nth i ports)(car (second m)))
                        (setf (nth i chans)(second (second m)))))))
    (mki 'chord
         :LMidic (lmidic obj)
         :Lvel (lvel obj)
         :Loffset (loffset obj)
         :Ldur (ldur obj)
         :Lchan chans
         :Lport ports)))



(om::defmethod! map-channel ((obj chord-seq) (mapping list))
(mki 'chord-seq
     :lmidic (loop for ch in (chords obj)
                       collect (map-channel ch mapping))
     :lonset (lonset obj)
     :legato (legato obj)))



(om::defmethod! map-channel ((obj multi-seq) (mapping list))
             
   (mki 'multi-seq
        :chord-seqs (loop for chseq in (chord-seqs obj)
                          collect (map-channel chseq mapping))))





(om::defmethod! implosion ((obj chord-seq)  )
:initvals '(nil  )
  :icon 136
  :doc  "transforms a chord-seq (or multi-seq) into a chord  . Change onsets into offsets. Useful for iterations, trills, etc...
as it should simplify internal representation . "

  (let ((lnotes (seq->notes obj)))
  
      (mki  'chord 
      :lmidic (mapcar  'first lnotes)
      :lvel  (mapcar 'fourth lnotes)
      :loffset   (mapcar 'second lnotes)                                              
      :ldur  (mapcar 'third lnotes)
      :lchan (mapcar 'fifth lnotes)
      :lport  (mapcar  'sixth lnotes)
      )))


(om::defmethod! implosion ((obj multi-seq)  )
:initvals '(nil  )
  :icon 136

  (implosion (mixer obj nil)))




(om::defmethod! portchan ((object container)  (port t)  (chan t))
  :initvals '(nil  1 1 )
  :icon 136
  :doc  "donne ‡ l'objet le port <port> et le canal <chan>"
  (ch-modif (ch-modif object '= port 'lport) '= chan 'lchan))



     

         

(om::defmethod! iso-dur ((object chord-seq) (dur number) (mode t))
  :initvals '(nil 200  'durtot)
  :icon 136
  :menuins (list (list 2 '(("interval" 'interv) ("durtotal" 'durtot) )))
  :doc  "place les notes d'un chord-seq ‡ intervalles rÈguliers; on donne l'intervalle de temps entre 2 notes,
 ou la durÈe totale. Les durÈes sont Ègales aux intervalles de durÈe; dans l'option 'interval' la derniËre 
durÈe est conservÈe comme dans le ch-seq original. Accepte multi-seq.
Faire prÈcÈder d'un align-chords si nÈcessaire"

  (let ((res (if (eq mode 'interv)
                 (mki 'chord-seq
                  :LMidic  (lmidic object)  
                  :Lchan  (lchan object)
                  :Lvel (lvel object)
                  :LOffset (loffset object)
                  :Ldur  (ldur object)
                 :lonset  (x-arithm 0 dur (length (ldur object))))
                  (mki 'chord-seq
                  :LMidic  (lmidic object)  
                  :Lchan  (lchan object)
                  :Lvel (lvel object)
                  :LOffset (loffset object)
                  :Ldur  (om-round (create-list (length (ldur object)) (om/ dur (length (ldur object)))))
                 :lonset  (om-round (dx->x 0 (create-list (length (ldur object)) (om/ dur (length (ldur object))))))))))

    (newport   (lier res) (lport object))))


(om::defmethod! iso-dur ((object multi-seq) (dur number) (mode t))
  :initvals '(nil 200  'durtot)
  :icon 136
  :menuins (list (list 2 '(("interval" 'interv) ("durtotal" 'durtot) )))

   (let* ((res ))
     (dolist (seq (chord-seqs object))
       (push (iso-dur seq dur mode) res    ))
     (mki 'multi-seq :chord-seqs (reverse res))))





(om::defmethod! time-vocoder ((object chord-seq) (reservoirs chord-seq) 
                            &optional (mode 'midic))

 :initvals '(nil nil 'midic)
   :menuins (list ( list 2 '(("Midics" 'midic) ("Freqs" 'freqs))))
   :indoc '("object" "rÈservoir" "mode")
   :icon 137
   :doc  "applique le contenu du ch-seq <object> sur la succession de champs harmoniques <reservoir>.
 i.e. chaque note de <object> sera 'accordÈe' sur le champ harmonique correspondant au mÍme instant.
<reservoir> doit avoir une duree au moins Ègale ‡ celle de  <object> "

(let* ((obj (explosion object))
       (obj-onsets (lonset obj))
       (champs (chords reservoirs))
      (intervalles  (lonset reservoirs))
      voconotes)

(loop  for  ch in (chords obj )
       for ons in obj-onsets
       do (push (ch-vocoder ch (lmidic (nth-obj reservoirs (cherche-intervalle ons intervalles)))) voconotes))
   
(mki 'chord-seq 
        :lmidic (nreverse voconotes)
        :lonset obj-onsets
        :legato (legato object))))
       
       

(om::defmethod! time-vocoder ((object multi-seq) (reservoirs chord-seq) 
                            &optional (mode 'midic))

(let ((res))
(loop for seq in (chord-seqs object)
       do (push (time-vocoder seq reservoirs mode) res))
(mki 'multi-seq :chord-seqs (reverse res))))
      





(defun cherche-intervalle (nb intervalles)
  (let* ((long (- (length intervalles) 2)) res)
    (for (i 0 1 long)
         (if (and (>= nb (nth i intervalles)) (< nb (nth (1+  i) intervalles))) (setq res i)))
    res))




(om::defmethod! triller ((chord chord) (interval number) (freq number) (dur number) )
  
  :initvals '(nil 100 10 1000 )
  :icon 136
  :doc  "create trills on all the notes of <chord> . interval of trill given by <interval> (midic)
 <freq> in hz, <dur> in msec .
velocity, channel and port identical to 1st note of <chord>"
  
  (let* ((lch (chords  (explosion chord)))
         (ltrilles (loop for n in lch
                         collect (trille (mixer (list n (om+ n interval)) nil) freq dur))))
    (mixer ltrilles nil)))





(om::defmethod! proliferer ((chseq chord-seq) (density number)  (ecart integer) )
  :initvals '(nil 1.5 200)
  :icon 136
  :doc  "add notes - number defined by density (float) - pitch defined by ecart (midic)) - other parameters=average of seq "
  
  (let* ((notes (lmidic chseq))
         (nbnewnotes (om-round (-  (* (length notes) density) (length notes))))
         (newmax (+ (list-max notes) ecart))
         (newmin (- (list-min notes) ecart))
         (resulseq chseq)
         (newnotes (om-round (n-arithm newmin newmax nbnewnotes)))
         (durmoy (list! (om-round (om-mean (flat (ldur chseq))))))
         (canal (first (lchan chseq)))
         (port (first (lport chseq)))
         (velo (list!  (om-round (om-mean (flat (lvel chseq))))))  )  

(print durmoy)
(print velo)
(print canal)
(print port)
            
    (dolist (n newnotes)
     (setq resulseq (random-insert n durmoy velo canal port resulseq)))
resulseq))


(defun random-insert (newnote durmoy velo canal port resulseq)
  (let* ((onsets (lonset resulseq))
       )
    (insert-object resulseq
                            (mki 'chord
                        :Lmidic (list newnote)
                        :LVel velo
                        :LDur durmoy
                        :LOffset (list 0)
                        :LChan canal
                        :Lport  port) (nth-random onsets))))


