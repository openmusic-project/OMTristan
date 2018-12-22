
; ------------------- OM->CS  fonctions personnelles --------------------
(in-package :om)


;------------------------------formant------------------------------------
;-------------------------------------------------------------------------

;from TM-librairie
;for compatibility

(defun seq->list (objet)
  "si c'est chseq retourne liste d'accords"
  (if (or (typep objet 'chord-seq) (typep objet 'chord)) (lmidic objet) objet))




;--------------------------------------------------------------------------

(defun formant1 (accord centre bande mulamp mode)
  (let* ((res (clone accord))
         (lnotes (lmidic accord))
        (min  (-  centre (/  bande 2.0)))  (max  (+ centre (/ bande 2)))
        (fctn (parabole/3 1 min 1 centre mulamp max 1)))

    (dolist (n lnotes)
      (let ((hauteur (if (equal mode 'freq) (mc->f (lmidic accord)) (lmidic accord))))
        (if (<>  hauteur  min max '<>)
          (set-slot n lvel  (round  (*  (lvel accord) (funcall fctn hauteur)))))))
  res))






;marche aussi pour les liste "numbers?" ds PW c-à-d les deux number et liste


(om::defmethod! formant ((accords chord-seq) 
                            (centre t)
                            (bande t)
                            (mulamp t)
                            (mode  symbol))
   
   
   :initvals (list t 6000 75 100 'freq)
   :indoc '("accords" "centre" "bande" "amps" "mulamp" "mode")
   :menuins '((4 (("freq" 'frq)
                 ("midic" 'midic))))
   :icon 132
   :doc "centre : fq ou midi central du formant
bande = largeur de bande totale du formant
mulamp = multiplicateur d'intensité"
   
   
   
   (let* (
          ;(accords (seq->list accords))
          (long (if (consp accords) (length accords))) res)
     
     (if (consp accords)
       (let ((centre (if (consp centre) centre (create-list long centre)))
             (bande (if (consp bande) bande (create-list long bande)))
             (mulamp (if (consp mulamp) mulamp (create-list long mulamp))))
         (loop for a in accords
               for c in centre
               for b in bande
               for m in mulamp
               do (push (formant1 a c b m mode) res)
               finally (return (nreverse res))))
       (formant1 accords centre bande mulamp mode))))



;Alreday defined in TM library in liste-analyse 
;(l-sum l-sum1 positions1 positions nbi-rec nbelem-ident length-1)






;------------------------------------gran-st-sco----------------------------------------------------

;gran-st-sco et granf-st-sco  modifiées (fichier  = optional)  - le type symbol ne fonctionnait pas. 
; Il faudrait changer les autres fct similaires


; fct corrigée par Laurent Pottier

(om::defmethod! gran-st-sco ((dates list) 
                                (tuile number) 
                                (frqs list) 
                                (amps list) 
                                (alea number)
                                (spatia list) 
                                &optional (fichier nil))


   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 'nil)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier" )
   :icon 132
   :doc ""

   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                    (* 2 last-ev))))
          nextDate newDate (fin (+ (last-elem dates) last-ev)))
     
     (format t "fenêtres : ")
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         
         (format file "f1 0 1024 10 1 ~%~%")
         
         (for  (i 0 1 long)
           (if (= (mod i 10) 0) (format t " ~A "  i) )
           (for (k 0 1 (1- (length (nth i frqs))))
             
             (format file "i1" )
             
             (setf nextDate (nth (1+ i) dates)
                   newDate  (+ (nth i dates) (* (/ alea 100.) (om-random 0 (- (nth (1+ i) dates)(nth i dates))))))

             (format file "~9,4F" newDate)   ; dates   
             (format file "~9,4F" (* tuile (- (nth (+ 2 i) dates) (nth i dates))) )  ; durées  
             (format file "~9,4F" (nth k (nth i amps)) )   ; amps
             (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
             (format file "~9,4F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
             (format file "~8,2F" (nth k (nth i spatia)) file)
             (princ #\newline  file)  ))
         
         (princ #\e  file)))
     
     (format t  "fichier ~A écrit  - durée ~8,3F ~%" nom fin))
   nil)


; nouvelle fct - les attaques à l'intérieur de chaque fenêtre sont systématiquement décalées
; pour réduire le bruit . Le paramètre alea devient inutile. Utiliser avec granArp-st.orc

(om::defmethod! gran-st-sco-arp ((dates list) 
                                (tuile number) 
                                (frqs list) 
                                (amps list) 
                                (spatia list) 
                                &optional (fichier nil))


   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 'nil)
   :indoc '("dates" "tuile" "frqs" "amps" "spatia" "fichier" )
   :icon 132
   :doc ""

   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          newDate
          (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                    (* 2 last-ev))))
          (fin (+ (last-elem dates) last-ev)))
     
     (format t "fenêtres : ")
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         
         (format file "f1 0 1024 10 1 ~%~%")
         (format file "f2 0 1024 -20 2 ~%~%")
         
         (for  (i 0 1 long)
           (if (= (mod i 10) 0) (format t " ~A "  i) )
           (for (k 0 1 (1- (length (nth i frqs))))
             
             (format file "i1" )

             (setf newDate (+ (nth i dates)(* (/ k (length (nth i frqs))) (- (nth (1+ i) dates)(nth i dates)))))

             (format file "~9,4F" newDate)   ; dates   
             (format file "~9,4F" (* tuile (- (nth (+ 2 i) dates) (nth i dates))) )  ; durées  
             (format file "~9,4F" (nth k (nth i amps)) )   ; amps
             (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
             (format file "~9,4F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
             (format file "~8,2F" (nth k (nth i spatia)) file)
             (princ #\newline  file)  ))
         
         (princ #\e  file)))
     
     (format t  "fichier ~A écrit  - durée ~8,3F ~%" nom fin))
 nil)


#|   ancienne fonction
(om::defmethod! gran-st-sco ((dates list) 
                                (tuile number) 
                                (frqs list) 
                                (amps list) 
                                (alea number)
                                (spatia list) 
                                &optional (fichier nil))


   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 'nil)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier" )
   :icon 132
   :doc ""

   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                    (* 2 last-ev))))
          (fin (+ (last-elem dates) last-ev)))
     
     (format t "fenêtres : ")
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         
         (format file "f1 0 1024 10 1 ~%~%")
         
         (for  (i 0 1 long)
           (if (= (mod i 10) 0) (format t " ~A "  i) )
           (for (k 0 1 (1- (length (nth i frqs))))
             
             (format file "i1" )
             (format file "~8,3F" (* (nth i dates) (+ 1 (/ (om-random (- alea) 
                                                                        (float alea)) 100)) ))   ; dates   
             (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth i dates))) )  ; durées  
             (format file "~9,4F" (nth k (nth i amps)) )   ; amps
             (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
             (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
             (format file "~8,2F" (nth k (nth i spatia)) file)
             (princ #\newline  file)  ))
         
         (princ #\e  file)))
     
     (format t  "fichier ~A écrit  - durée ~8,3F ~%" nom fin))
   nil)

|#



;------------------------------------granf-st-sco----------------------------------------------------

(defun spectre->fct (spectre)
  (let* ((rangs (first spectre))
        (last-harm (last-elem rangs))
        (intens (om-round (om-scale/max (second spectre) 1) 3))
        res)
    (for (i 1 1 last-harm)
      (push (l-nth intens (position i rangs)) res))
    (nreverse  (substitute 0 nil res  ))))


(om::defmethod! granf-st-sco ((dates list) 
                                 (tuile number) 
                                 (frqs list) 
                                 (amps list) 
                                 (alea number)
                                 (spatia list) 
                                 (spectre list)
                                 &optional (fichier nil) )
   
   
   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2)  '((1) (1) ) 'nil)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier" "spectre" )
   :icon 132
   :doc "Ècriture d'une partition granulaire avec table d'onde; par dÈfaut, onde sinus
format spectre : ( (rangs harm) (intensitÈs))"
   
   
   
   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                    (* 2 last-ev))))
          (fin (+ (last-elem dates) last-ev)))
     
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         
         (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )
         
         (for  (i 0 1 long)
           (for (k 0 1 (1- (length (nth i frqs))))
             
             (format file "i1" )
             (format file "~8,3F" (* (nth i dates) (+ 1 (/ (om-random (- alea) 
                                                                      (float alea)) 100)) ))   ; dates   
             (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth i dates))) )  ; durÈes  
             (format file "~9,4F" (nth k (nth i amps)) )   ; amps
             (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
             (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
             (format file "~8,2F" (nth k (nth i spatia)) file)
             (princ #\newline  file)  ))
         
         (princ #\e  file)))
     
     (format t  "fichier ~A Ècrit  - durÈe ~8,3F ~%" nom fin))
   nil)



;------------------------------------seq->gran-sco----------------------------------------------------

(om::defmethod!  spatia-freq ((freqs list) 
                            (min number) 
                            (max number))
   :initvals (list '(1 2) 0 1)
   :indoc '("freqs" "min" "max")
   :icon 132
   :doc "spatialise en fct des frÈq  entre min = gauche et max = droite
Si on entre des midics:  rÈpartition plus Ètale gauche -> droite"
   
   (om-round (om-scale freqs min max (list-min (list-min freqs)) (list-max (list-max freqs))) 2))

(defun  spatia-freq1 (freqs)
"spatialise en fct des frÈq
Si on entre des midics:  rÈpartition plus Ètale gauche -> droite"
         (om-round (om-scale freqs 0 1 (list-min freqs) (list-max freqs)) 2))


(om::defmethod!  spatia-rotor ((freqs list) 
                             (min number) 
                             (max number)) 
  :initvals (list '(1 2) 0 1)
  :indoc '("freqs" "min" "max")
  :icon 132
  :doc  "spatialise en fct des frÈq,(entre min et max) et mais ajoute une permutation
circulaire des spatialisations pendant toute la durÈe du son"
  
  (let ((freqs (om-round (om-scale freqs min max ) 2)) 
        (position (arithm-ser 0 1 (/ 1 (length freqs)) )) res)
    (loop for f in freqs
          for p in position
          do (push  (rotate f (round (* p (length f)))) res)
          finally (return (reverse res) ))))


(om::defmethod!  spatia-alea ((freqs list) 
                            (gauche number) 
                            (droite number))
  :initvals (list '(1 2) 0 1)
  :indoc '("freqs" "gauche" "droite")
  :icon 132
  :doc  "spatialise alÈatoirement entre les valeurs indiquÈes"

  (let ((res) (sublist))
    (dolist (f freqs)
      (repeat (length f) (push (random 100) sublist))
      (push sublist res)
      (setq sublist nil))
    (nreverse (om-round (om-scale res gauche droite) 2))))

(defun  spatia-alea1 (freqs)
"spatialise alÈatoirement "
  (let ((res) )
    (repeat (length freqs) (push (random 100) res))
   (nreverse (g-round  res 2) )))


(om::defmethod! seq->gran-sco ((accords chord-seq) 
                             (tuile number) 
                             (mulamp number) 
                             (mulmin number) 
                             (alea number)
                             (spatia symbol) 
                             (fichier list ) 
                             (spectre list))
   
   
   :initvals (list t 1 .5 1 0 "frq" 'nil '((1) (1)))
   :indoc '("accords" "tuile" "frqs" "amps" "alea" "spatia" "fichier" "spectre" )
   :menuins '((5 (("frq" 'frq)
                ("midic" 'midic)
                ("rotor" 'rotor)
                ("alea" 'alea))))
   :icon 132
   :doc "Ècriture d'une partition granulaire à partir d'un chord-seq
mulamp = scaling des amp : si mulamp = 1, somme des amp de chaque accord = 1
mulmin : redresse la courbe des amp (compresse si mulmin > 1)
table d'onde; par dÈfaut, onde sinus
format spectre : ( (rangs harm) (intensitÈs))"
        
  
   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (frqs (mc->f (lmidic accords)))
          (amps (lvel accords))
          (amps (om* (om/ amps (list-max (l-sum amps))) mulamp))  ; somme des amps = 1 * mulamp
          (maxamp (list-max (list-max amps)))
          (minamp (list-min (list-min amps)))
          (amps (om-round (om-scale amps (* minamp mulmin) maxamp minamp maxamp) 4)) ; redresser 
          ; courbe des amp  si mulmin <> 1
          (dates (om/ (butlast (lonset accords)) 1000.0)) ;maybe butlast isnot necessary??
          (offsets (om/ (loffset accords) 1000.0))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          (dates (x-append dates (+ (last-elem dates ) last-ev) (+ (last-elem dates ) (* 2 last-ev))))
          ; ajout de deux dates pour le calcul de la derniËre pente d'extinction
          (offsets (x-append  offsets (list (last-elem offsets) (last-elem offsets))))   ; mÍme raison
          (attaques (om+  offsets dates))    
          (attaques (om- attaques (list-min (list-min attaques))))  ; pour commencer le temps ‡ 0
          (space (case spatia
                   (frq (spatia-freq frqs 0 1))
                   (midic (spatia-freq (lmidic accords) 0 1))
                   (rotor (spatia-rotor frqs 0 1))
                   (alea (spatia-alea frqs 0 1))))
          (fin (+ (last-elem dates) last-ev)))
   
    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )

        (for  (i 0 1 long)
          (for (k 0 1 (1- (length (nth i frqs))))
            (format file "i1" )
            (format file "~8,3F" (* (nth k (nth i attaques)) (+ 1 (/ (om-random (- alea) 
                                                           (float alea)) 100)) ))   ; dates   
            (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth i dates))) )  ; durÈes  
            (format file "~9,4F" (nth k (nth i amps)) )   ; amps
            (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
            (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
            (format file "~8,2F" (nth k (nth i space)) file)
            (princ #\newline  file)  ))

        (princ #\e  file)))
    (format t  "fichier ~A Ècrit  - durÈe ~8,3F ~%" nom fin) )
    nil)


;------------------------------------seq->ord-sco----------------------------------------------------


(om::defmethod! seq->ord-sco ((accords chord-seq) 
                            (muldur number) 
                            (mulamp number) 
                            (mulmin number) 
                            (alea number) 
                            (spatia symbol) 
                            (fichier list) 
                            (spectre list))

   
   :initvals (list t 1 .5 1 0 "frq" 'nil '((1) (1)))
   :indoc '("accords" "muldur" "mulamp" "mulmin" "alea" "spatia" "fichier" "spectre" )
   :menuins '((5 (("frq" 'frq)
                ("midic" 'midic)
                ("rotor" 'rotor)
                ("alea" 'alea))))
   :icon 132
   :doc "Ècriture d'une partition ordinaire à partir d'un chord-seq
les durÈes seront celles du chseq, multipliÈes par muldur
mulamp = scaling des amp : si mulamp = 1, somme des amp de chaque accord = 1
mulmin : redresse la courbe des amp (compresse si mulmin > 1)
les offsets sont pris en compte
table d'onde; par dÈfaut, onde sinus
format spectre : ( (rangs harm) (intensitÈs))"
        
  
  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
         (muldur (/ muldur 1000)); might be in milliseconds
         (frqs (mc->f (lmidic accords)))
         (amps (lvel accords))
         (amps (om* (om/ amps (list-max (l-sum amps))) mulamp))  ; somme des amps = 1 * mulamp
         (maxamp (list-max (list-max amps)))
         (minamp (list-min (list-min amps)))
         (amps (om-round (om-scale amps (* minamp mulmin) maxamp minamp maxamp) 4)) ; redresser 
                                                 ; courbe des amp  si mulmin <> 1
         (dates (om/ (butlast (lonset accords)) 1000.0))
         (offsets (om/ (loffset accords) 1000.0))
         (durÈes (om* (ldur accords) muldur))
         (long (1- (length dates)))
         ;(last-ev (- (nth long dates) (nth (1- long) dates)))
        ; (dates (x-append dates (+ (last-elem dates ) last-ev) (+ (last-elem dates ) (* 2 last-ev))))
        ;                       ; ajout de deux dates pour le calcul de la derniËre pente d'extinction
        ; (offsets (x-append  offsets (list (last-elem offsets) (last-elem offsets))))   ; mÍme raison
         (attaques (om+  offsets dates))    
         (attaques (om- attaques (list-min (list-min attaques))))  ; pour commencer le temps ‡ 0
         (space (case spatia
                  (frq (spatia-freq frqs 0 1))
                  (midic (spatia-freq (lmidic accords) 0 1))
                  (rotor (spatia-rotor  frqs 0 1))
                  (alea (spatia-alea frqs 0 1))))
         (fin (+ (last-elem dates) (list-max (last-elem durÈes)))))
   
    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )

        (for  (i 0 1 long)
          (for (k 0 1 (1- (length (nth i frqs))))
            (format file "i1" )
            (format file "~8,3F" (* (nth k (nth i attaques)) (+ 1 (/ (om-random (- alea) 
                                                           (float alea)) 100)) ))   ; dates   
            (format file "~8,3F" (nth k (nth i durÈes)) )  ; durÈes  
            (format file "~9,4F" (nth k (nth i amps)) )   ; amps
            (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
         ;   (format file "~8,3F" (* tuile (- (nth (+ 2 i) dates) (nth (1+ i) dates))) ) ; pentes
            (format file "~8,2F" (nth k (nth i space)) )
            (princ #\newline  file)  ))

        (princ #\e  file)))
    (format t  "fichier ~A Ècrit  - durÈe ~8,3F ~%" nom fin) )
    nil)


;------------------------------------chord->gran-sco----------------------------------------------------

(om::defmethod! chord->ord-sco ((accord chord)
                              (muldur number) 
                              (mulamp number) 
                              (mulmin number) 
                              (alea number)
                              (spatia symbol) 
                              (fichier list)
                              (spectre list))


   
   :initvals (list t 1 .5 1 0 "frq" 'nil '((1) (1)))
   :indoc '("accord" "muldur" "mulamp" "mulmin" "alea" "spatia" "fichier" "spectre" )
   :menuins '((5 (("frq" 'frq)
                ("midic" 'midic)
                ("alea" 'alea))))
   :icon 132
   :doc "Ècriture d'une partition ordinaire  ‡ partir d'un accord
les durÈes seront celles de l'accord, multipliÈes par muldur
mulamp = scaling des amp : si mulamp = 1, somme des amp de chaque accord = 1
(fct ''deformer%''  : si  ?amp < 50  : creuse les Ècarts)
table d'onde; par dÈfaut, onde sinus
format spectre : ( (rangs harm) (intensitÈs))"
        
 
  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
         (muldur (/ muldur 1000))
         (frqs (mc->f (lmidic accord)))
         (amps (lvel accord))
         (maxamp (list-max (list-max amps)))
         (minamp (list-min (list-min amps)))
         (amps (om-round (om-scale amps (* minamp mulmin) maxamp minamp maxamp) 4)) ; redresser 
                                                 ; courbe des amp  si mulmin <> 1
         (amps (om* (om/ amps (list-max (l-sum amps))) mulamp))  ; somme des amps = 1 * mulamp
         (offsets (om/ (loffset accord) 1000))
         (durÈes (om* (ldur accord) muldur))    
         (attaques (om- offsets  (list-min offsets)))
         (fin (list-max (om+ offsets durÈes)))  ; pour commencer le temps ‡ 0
         (space (case spatia
                  (frq (spatia-freq1 frqs))
                  (midic (spatia-freq1 (lmidic accord)))
                  (alea (spatia-alea1 frqs)))))
         
   
    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )

        (for (i 0 1 (1- (length  frqs)))
            (format file "i1" )
            (format file "~8,3F" (* (nth i attaques) (+ 1 (/ (om-random (- alea) 
                                                           (float alea)) 100)) ))   ; dates   
            (format file "~8,3F"  (nth i durÈes))  ; durÈes  
            (format file "~9,4F"  (nth i amps))  ; amps
            (format file "~9,2F"  (nth i frqs))   ; freqs  
            (format file "~8,2F"  (nth i space) file)
            (princ #\newline  file)  )

        (princ #\e  file)))
    (format t  "fichier ~A Ècrit  - durÈe ~8,3F ~%" nom fin) )
    nil)

;--------------------------------------------STEREO-SCO------------------------------------------

(om::defmethod! stereo-sco ((dates list) 
                               (durs number) 
                               (frqs list) 
                               (amps list) 
                               (alea number)
                               (spatia list) 
                               (fichier symbol)
                               (spectre list))
   
   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 'nil '((1) (1) ))
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier" "spectre")
   :icon 132
   :doc "écriture d'une partition normale (non granulaire) stereo avec 
table d'onde à partir de listes (dates,durs, freqs, amps, spatia) 
(il s'agit d'une séquence d'accords)
format spectre : ( (rangs harm) (intensités))"
   
   
   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (long (1- (length dates)))
          (last-ev (- (nth long dates) (nth (1- long) dates)))
          (dates (x-append dates (+ (last-elem dates ) last-ev)  ))
          (accords? (consp (first frqs))))
     
     (when nom
       (with-open-file (file nom :direction :output 
                             :if-exists :supersede)
         
         (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )
         
         
         (if   accords?
           
           (for  (i 0 1 long)
             (for (k 0 1 (1- (length (nth i frqs))))
               
               (format file "i1" )
               (format file "~8,3F" (* (nth i dates) (+ 1 (/ (om-random (- alea) 
                                                                          (float alea)) 100)) ))   ; dates   
               (format file "~8,3F" (nth k (nth i durs)) )  ; durées  
               (format file "~9,4F" (nth k (nth i amps)) )   ; amps
               (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
               (format file "~8,2F" (nth k (nth i spatia)) )
               (princ #\newline  file)  ))
           
           (for  (i 0 1 long)
             (format file "i1" )
             (format file "~8,3F" (* (nth i dates) (+ 1 (/ (om-random (- alea) 
                                                                        (float alea)) 100)) ))   ; dates   
             (format file "~8,3F" (nth i durs))   ; durées  
             (format file "~9,4F" (nth i amps))   ; amps
             (format file "~9,2F" (nth i frqs))   ; freqs  
             (format file "~8,2F" (nth i spatia)) 
             (princ #\newline  file)  )
           )
         
         (princ #\e  file)))
     
     (format t  "fichier ~A écrit ~%" nom))
   nil)

;--------------------------------------------granatt-st-sco-----------------------------------


; nouvelle fct incluant modif de LP

(om::defmethod! granatt-st-sco-arp ((dates list) 
                                   (tuile number) 
                                   (frqs list)
                                   (amps list) 
                                    
                                   (spatia list) 
                                   (fichier list) 
                                   (spectre list))

   :initvals (list '(1 2) 1 '(1 2) '(1 2)  '(1 2) 'nil '((1) (1) ))
   :indoc '("dates" "tuile" "frqs" "amps"  "spatia" "fichier" "spectre")
   :icon 132
   :doc "écriture d'une partition granulaire avec attaque 
avec table d'onde; par défaut, onde sinus
format spectre : ( (rangs harm) (intensités))
l'attaque utilise ins1, le reste ins2"


  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
        (long (1- (length dates)))
        (last-ev (- (nth long dates) (nth (1- long) dates)))
        (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                  (* 2 last-ev))))
        
        (dates  (om- dates (first dates)))
        (dur0 (* tuile  (second dates) ))
        (fin (+ (last-elem dates) last-ev)))


    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )
      
;-------  premier échantillon (attaque directe) -------
        (format t "fenêtres : 1  ")
            (for (k 0 1 (1- (length (first frqs))))   
              (format file "i1" )
              (format file "~8,3F" 0.00)   ; date   
              (format file "~8,3F" dur0)   ; durée
              (format file "~9,4F" (nth k (first amps)) )   ; amps
              (format file "~9,2F" (nth k (first frqs)) )   ; freqs  
              (format file "~8,3F" dur0)  ; pente
              (format file "~8,2F" (nth k (first spatia)) )
              (princ #\newline  file)  )

;-------  suite des échantillons  ----------
          
        (for  (i 1 1 long)         
          (if (= (mod i 10) 0) (format t " ~A "  i) )

          (let* ((dati (nth i dates))
                 (nextdati (nth (1+ i) dates))
                 (pentedroite (* tuile (- (nth (1+ i) dates) dati)))
                 (pentegauche (* tuile (- dati (nth  (1- i) dates))))
                 (nextpentegauche (* tuile (- nextdati dati)))
                 (pentegauche (min dati pentegauche ))
                 (nextpentegauche (min nextdati nextpentegauche ))
                 (duri  (+ pentegauche pentedroite))
                 (attaque (- dati pentegauche))
                 (nextattaque (- nextdati nextpentegauche))


                 newdate)
            
            (for (k 0 1 (1- (length (nth i frqs))))
              
              (format file "i2" )

              (setf newDate (+  attaque (* (/ k (length (nth i frqs))) (- nextattaque attaque))))
              (format file "~9,4F" newDate)   ; dates   
              (format file "~9,3F" duri ) ; durées  
              (format file "~9,4F" (nth k (nth i amps)) )   ; amps
              (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
              (format file "~9,3F" pentedroite ) 
              (format file "~8,2F" (nth k (nth i spatia)) )
              (princ #\newline  file)  )))
          
          (princ #\e  file)))

    (format t  "~% fichier ~A écrit  - durée ~8,3F ~%" nom fin))
    nil)




(om::defmethod! granatt-st-sco ((dates list) 
                                   (tuile number) 
                                   (frqs list)
                                   (amps list) 
                                   (alea number) 
                                   (spatia list) 
                                   (fichier list) 
                                   (spectre list))

   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 'nil '((1) (1) ))
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier" "spectre")
   :icon 132
   :doc "écriture d'une partition granulaire avec attaque 
avec table d'onde; par défaut, onde sinus
format spectre : ( (rangs harm) (intensités))
l'attaque utilise ins1, le reste ins2
offset: le son peut démarrer après un délai"


  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
        (long (1- (length dates)))
        (last-ev (- (nth long dates) (nth (1- long) dates)))
        (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                  (* 2 last-ev))))
        
        (dates  (om- dates (first dates)))
        (dur0 (* tuile  (second dates) ))
        (fin (+ (last-elem dates) last-ev))
        )

    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )
      
;-------  premier échantillon (attaque directe) -------
        (format t "fenêtres : 1  ")
            (for (k 0 1 (1- (length (first frqs))))   
              (format file "i1" )
              (format file "~8,3F" 0.00)   ; date   
              (format file "~8,3F" dur0)   ; durée
              (format file "~9,4F" (nth k (first amps)) )   ; amps
              (format file "~9,2F" (nth k (first frqs)) )   ; freqs  
              (format file "~8,3F" dur0)  ; pente
              (format file "~8,2F" (nth k (first spatia)) )
              (princ #\newline  file)  )

;-------  suite des échantillons  ----------
          
        (for  (i 1 1 long)         
          (format t " ~A " (1+ i) )

          (let* ((dati (nth i dates))
                 (pentedroite (* tuile (- (nth (1+ i) dates) dati)))
                 (pentegauche (* tuile (- dati (nth  (1- i) dates))))
                 (pentegauche (min dati pentegauche ))
                 (duri  (+ pentegauche pentedroite))
                 (attaque (- dati pentegauche))  ) 
            
            (for (k 0 1 (1- (length (nth i frqs))))
              
              (format file "i2" )
              (format file "~8,3F" (* attaque (+ 1 (/ (om-random (- alea) 
                                                     (float alea)) 1000)) ))   ; dates   
              (format file "~8,3F" duri ) ; durées  
              (format file "~9,4F" (nth k (nth i amps)) )   ; amps
              (format file "~9,2F" (nth k (nth i frqs)) )   ; freqs  
              (format file "~8,3F" pentedroite ) 
              (format file "~8,2F" (nth k (nth i spatia)) )
              (princ #\newline  file)  )))
          
          (princ #\e  file)))

    (format t  "~% fichier ~A écrit  - durée ~8,3F ~%" nom fin))
    nil)


;----------------------------------voix-granatt----------------------------------------

(om::defmethod! voix-granatt ((dates list) 
                                 (tuile number) 
                                 (frqs list) 
                                 (amps list) 
                                 (alea number) 
                                 (spatia list)  
                                 (offset number))

   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 0)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "offset")
   :icon 132
   :doc "fabrication d'une voix de partition granulaire avec attaque 
permet polyphonie de sons; offset donne le délai d'attaque de la voix
brancher dans poly-granatt-sco"



  
  (let* ((long (1- (length dates)))
         (last-ev (- (nth long dates) (nth (1- long) dates)))
         (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                   (* 2 last-ev))))
         (dates  (om- dates (first dates)))
         (dur0 (* tuile  (second dates) ))
         (fin (+ (last-elem dates) last-ev offset))
         attaques durees pentesdroites )
    
    ;-------  premier échantillon (attaque directe) -------
    
    
    (push  (create-list (length (first frqs)) offset) attaques)
    (push dur0 durees)
    (push dur0 pentesdroites)
    
    ;-------  suite des échantillons  ----------
    
    (for  (i 1 1 long)
      (let* ((dati (nth i dates))
             (pentedroite (* tuile (- (nth (1+ i) dates) dati)))
             (pentegauche (* tuile (- dati (nth  (1- i) dates))))
             (pentegauche (min dati pentegauche ))
             (attaque (- dati pentegauche))
             attaqui )
        
        (for (k 0 1 (1- (length (nth i frqs))))
          (push  (+ offset (* attaque (+ 1 (/ (om-random (- alea) 
                      (float alea)) 1000)) )) attaqui)  ; dates soumises à aléa
          )                                            ; (donc différentes pour chaque partiel)
        
        ;(push (nreverse attaqui) attaques)
        (push attaqui attaques)     ; l'ordre n'a pas d'importance puisque c'est aléatoire
        (push (+ pentegauche pentedroite) durees )  
        (push  pentedroite pentesdroites ) ) )
    
    (list (nreverse attaques) (nreverse durees) amps frqs (nreverse pentesdroites) 
          spatia fin)
    )) ; durees et pentesdroites sont des listes simples (chaque fenêtre a les mêmes
       ; valeurs)  - les autres sont des listes de listes (chaque partiel est différent)

;----------------------------------poly-granatt-sco---------------------------------------

(om::defmethod! poly-granatt-sco ((fichier list)
                                     (spectre list)
                                     (son1 list) 
                                     (son2 list)
                                     &rest sons)
   
   :initvals (list nil '((1)(1)) nil nil nil)
   :indoc '("fichier" "spectre" "son1" "son2" "sons")
   :icon 132
   :doc "écriture d'une partition polyphonique granulaire avec attaque 
avec table d'onde; par défaut, onde sinus - format spectre : ( (rangs harm) (intensités))
l'attaque utilise ins1, le reste ins2
composition des listes ''sons'' : attaques durées amps freqs pentesdroites spatias  fin
"
   
   
   
   (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          
          (sons (x-append (if (null son2) son1 (list son1 son2 )) sons))
          (fin (list-max (mapcar 'seventh sons)))
          (compte 0))
     
     (when nom
       (with-open-file (file nom :direction :output :if-exists :supersede)
         
         (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )
         
         ;  ======   boucle des sons  ======
         
         (dolist ( s sons)
           (setq compte (1+ compte))
           (let ((long (1- (length (second s)))))
             (format t "~%")
             (format t "son ~A  - fenêtres: " compte )
             
             ; ---- boucle des fenêtres de chaque son ----
             (for  (i 0 1 long)
               (let ((instr (if (= i 0) "i1" "i2"))
                     (longfenetre (1- (length (nth i (fourth s))))))
                 (format t "- ~A "  i)
                 
                 (for (k 0 1 longfenetre)
                   (format file instr)
                   (format file "~8,3F" (nth k (nth i (first s)) ))  ; date   
                   (format file "~8,3F" (nth i (second s)))   ; durée
                   (format file "~9,4F" (nth k (nth i (third s)) )) ; amps
                   (format file "~9,2F" (nth k (nth i (fourth s))))  ; freqs  
                   (format file "~8,3F" (nth i (fifth s)))  ; pente
                   (format file "~8,2F" (nth k (nth i (sixth s))) )
                   (princ #\newline  file)  )))  )
           
           (princ #\newline  file)  )
         
         (princ #\e  file)))
     
     (format t  "~% fichier ~A écrit  - durée ~8,3F ~%" nom fin))
   nil)



;----------------------------------voix-granul----------------------------------------------

; ..............  polyphonie en construisant d'abord une ou plusieurs voix
; ..............  puis en les regroupant pour les écrire dans poly-granul-sco
; .............. ceci ne marche que pour peu de voix, ou pour des sons courts
; .............. ( crée de longues listes intermédiaires)

(om::defmethod! voix-granul ((dates list) 
                                (tuile number)
                                (frqs list)
                                (amps list) 
                                (alea number) 
                                (spatia list)  
                                (offset number))
   
   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) 0)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "offset")
   :icon 132
   :doc "fabrication d'une voix de partition granulaire sans attaque 
permet polyphonie de sons; offset donne le délai d'attaque de la voix
brancher dans poly-granul-sco"
  

  
  (let* ((long (1- (length dates)))
         (last-ev (- (nth long dates) (nth (1- long) dates)))
         (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                   (* 2 last-ev))))
         (dates  (om- dates (first dates)))
         (dur0 (* tuile  (second dates) ))
         
         (fin (+ (last-elem dates) last-ev offset))
         attaques durees pentesdroites )
    
    ;-------  premier échantillon  -------
    

    (push dur0 durees)
    (push dur0 pentesdroites)
    (let ((dat0 (- offset (* 2 dur0))) attaq0 )
      (for (k 0 1 (1- (length (first frqs))))
        (push (* dat0 (+ 1 (/ (om-random (- alea) (float alea)) 1000)) ) attaq0))
      (push attaq0 attaques))
    
    ;-------  suite des échantillons  ----------
    
    (for  (i 1 1 long)
      (let* ((dati (nth i dates))
             (pentedroite (* tuile (- (nth (1+ i) dates) dati)))
             (pentegauche (* tuile (- dati (nth  (1- i) dates))))
             ;(pentegauche (min dati pentegauche ))
             (attaque (- dati pentegauche))
             attaqui )
        
        (for (k 0 1 (1- (length (nth i frqs))))
          (push  (+ offset (* attaque (+ 1 (/ (om-random (- alea) 
                      (float alea)) 1000)) )) attaqui)  ; dates soumises à aléa
          )                                            ; (donc différentes pour chaque partiel)
        
        (push  attaqui attaques)
        (push (+ pentegauche pentedroite) durees )  
        (push  pentedroite pentesdroites ) ) )
    
    (list (om- (nreverse attaques) (list-min (first attaques))) 
          (nreverse durees) amps frqs (nreverse pentesdroites) 
          spatia fin)
    ))   ; durees et pentesdroites sont des listes simples (chaque fenêtre a les mêmes
          ; valeurs)  - les autres sont des listes de listes (chaque partiel est différent)



;-----------------------------poly-granul-sco-----------------------------------------------


(om::defmethod! poly-granul-sco ((fichier list)
                                     (spectre list)
                                     (son1 list) 
                                     (son2 list)
                                     &rest sons)

   :initvals (list nil '((1)(1)) nil nil nil)
   :indoc '("fichier" "spectre" "son1" "son2" "sons")
   :icon 132
   :doc "écriture d'une partition polyphonique granulaire sans attaque 
avec table d'onde; par défaut, onde sinus - format spectre : ( (rangs harm) (intensités))
composition des listes ''sons'' : attaques durées amps freqs pentesdroites spatias  fin
"
   


  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
         
         (sons (x-append (if (null son2) son1 (list son1 son2 )) sons))
         (fin (list-max (mapcar 'seventh sons)))
         ( compte 0))

    (when nom
      (with-open-file (file nom :direction :output :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )

;  ======   boucle des sons  ======

        (dolist ( s sons)
          (setq compte (1+ compte))
          (let ((long (1- (length (second s)))))
            (format t "~%")
            (format t "son ~A  - fenetres: " compte )

            ; ---- boucle des fenêtres de chaque son ----
            (for  (i 0 1 long)
              (let ((longfenetre (1- (length (nth i (fourth s))))))
                (format t "- ~A "  i)

                (for (k 0 1 longfenetre)
                  (format file "i1")
                  (format file "~8,3F" (nth k (nth i (first s)) ))  ; date   
                  (format file "~8,3F" (nth i (second s)))   ; durée
                  (format file "~9,4F" (nth k (nth i (third s)) )) ; amps
                  (format file "~9,2F" (nth k (nth i (fourth s))))  ; freqs  
                  (format file "~8,3F" (nth i (fifth s)))  ; pente
                  (format file "~8,2F" (nth k (nth i (sixth s))) )
                  (princ #\newline  file)  )))  )

          (princ #\newline  file)  )
            
        (princ #\e  file)))
        
     (format t  "~% fichier ~A écrit  - durée ~8,3F ~%" nom fin))
      nil)



; ..................................................................................


;  création de partitions polyphoniques en écrivant directement le fichier
;  à partir d'une analyse, et d'un accord qui contient les données
;  (hauteur, durée, ampli, offset)



; vérifier poly-spatia-rotor

(defun poly-spatia-rotor (num frq min max ) 
  (let ((frq (om-round (om-scale frq min max ) 2)) 
         )
    (permut-circ frq num)))

   


(defun  poly-spatia-alea  (frq gauche droite ) 
  (let ((res))
    (repeat (length frq)
      (push  (random 100.0) res))
    (om-round (om-scale res gauche droite) 2)))



(defun poly-spatia (num frq spatia mode minn maxn)
  (let* ((frq (if (= 1 mode) frq (f->mc frq)))
        (res (cond ((= spatia 1) (spatia-freq  frq minn maxn))
                   ((= spatia 2) (poly-spatia-rotor num frq minn maxn))
                   ((= spatia 3) (poly-spatia-alea frq minn maxn)))))
    res))
    
;----------------------------chord->polygranul---------------------------------------------

; Note on posn-match:
; In PW the second argument is a list or an atom.
; In OM it is only a list
; There's another way to use it with an atom by using do-posn-match
; So Om team must decide, wether to create a new method
; for posn-match accepting as a second argument an atom.

(om::defmethod! chord->polygranul ((fichier list)
                                      (accord chord) 
                                      (analyse list)
                                      (tuile number) 
                                      (alea number)
                                      (fqfond number)
                                      (spatia symbol)
                                      (mode symbol)
                                      (minspa number) 
                                      (maxspa number)
                                      (spectre list)
                                      (debut symbol)
                                      (limite number))


   :initvals (list nil t '(1 2) 1 0 440 'pitch 'midic 0 1 '((1) (1)) 'doux 22000)
   :indoc '("fichier" "chord" "analyse" "tuile"
            "alea" "fqfond" "spatia"  "mode"
            "minspa" "maxspa" "spectre"  "début" "limite")
   :menuins '((6 (("pitch" 'pitch)
                 ("rotor" 'rotor)
                 ("alea" 'alea)))
             (7 (("midic" 'midic)
                 ("frq" 'frq)))
             (11 (("doux" 'doux)
                  ("percu" 'percu))))
   :icon 132
   :doc " construit une séquence de sons resynthétisés selon les données d'analyse
 <analyse>  (composition des listes <analyse> : dates freqs amps)
 chaque son est transposé, décalé, et modulé en amplitude selon les
hauteurs, offsets et vélo. des notes de <accord>
fqfond: fond estimée de l'analyse
minspa, maxspa : valeurs min et max de spatia (entre 0 et 1) - peuvent être des
listes (de longueur = à nb fenêtres)
mode : la spatia peut être proportionnelle aux midic ou aux freq
spectre; par défaut, onde sinus - format spectre : ( (rangs harm) (intensités))
début : le son peut débuter avec pente douce ou tel qu'il est dans l'analyse (percu)
utiliser gran-st ou granatt-st respectivement
limite: fq maximale (pour éviter repliement)
"


   (let* ((spatia (case spatia
                    (pitch 1)
                    (rotor 2)
                    (alea 3)))
          (mode (case mode
                  (midic 1)
                  (frq 2)))
          (debut (case debut 
                   (doux 1)
                   (percu 2)))
          
          (nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
          (lnotes (inside accord)) ; ATTENTION ici il faut garder les notes en tant qu'objet pour le traitement plutard
          (longnotes (1- (length lnotes)))
          (datanal (first analyse))                   ; liste des dates fenêtres d'analyse
          (datanal  (om- datanal (first datanal)))      ; pour commencer à 0
          (nbfen (1- (length datanal)))
          (last-ev (- (nth nbfen datanal) (nth (1- nbfen) datanal)))
          (duranal (+ (last-elem datanal ) last-ev))    ; durée analyse (extrapol dernière fen)
          (datanal (x-append datanal duranal  (+ duranal  last-ev)))
          
          (offsets (om/ (loffset accord) 1000))
          (durees  (om/ (ldur accord 3) 1000))
          
          ;       (first-dur (list-max (do-posn-match (durées (positions (offsets (list-min offsets)))))))
          ; durée de la note qui apparaît en premier (de la + longue si plusieurs
          ; apparaissent en même temps)
          ;      (first-muldur (/ first-dur duranal))  ; mul durée 1e note
          ;     (first-pentegauche (* tuile (* first-ev first-muldur)))
          (offsets  (om- offsets (list-min offsets)) ) ; pour commencer à 0
          
          (fin (list-max (om+ offsets durees)))
          ; estimation grossière durée son synthétisé (sans considérer tuilage)
          )

    (when nom
      (with-open-file (file nom :direction :output :if-exists :supersede)
    
        (format file "f1 0 1024 10 ~{~8,4F~} ~%~%" (spectre->fct spectre) )

;  ======   boucle des notes  ======

        (for (n 0 1 longnotes)
          
          (format t "~%")
          (format t "note ~A  - fenêtres: " (1+ n) )

          (let* ((muldur (/ (nth n durees) duranal))
                (multuile (* muldur tuile))    ; rapport durée voulue/durée analy * tuilage
                (offn (nth n offsets))
                (note (nth n lnotes))
                (transpo (/ (mc->f (midic note)) fqfond))
                (mulamp (/ (vel note) 100))  ; vel 100 considere comme val moyenne
                )

          ; ---- boucle des fenetres de chaque note ----

              ;-------  premier echantillon  -------
            
            (let* ((longfenetre (1- (length (first (second analyse)))))
                  (instrument (if (= debut 1) 2 1)) ;si début sans attaque i2 sinon i1
                  (datedeb (if (= debut 1) offn (+ offn (* muldur (second datanal)))))
                                  ; date : si le 1er echantillon est percussif
                                  ; il debute en meme temps que le 2nd
                  (pente0 (* multuile (second datanal)))
                  (dur0 (if (= debut 1) (* 2 pente0) pente0))
                                  ; si début sans attaque il y a une pentegauche
                                 ; (et on estime penteg = pented) sinon durée=pente
                  (mulampcorr (if (= debut 2) (* mulamp tuile) mulamp)) 
                      ; nouveau !!! si début percussif, il faut renforcer le premier
                      ; échantillon proportionnellement au tuilage
                  (coefspa (poly-spatia 0 (first (second analyse)) spatia mode
                                      (if (atom minspa) minspa (nth n minspa))
                                      (if (atom minspa) maxspa (nth n maxspa)))))

              (for (k 0 1 longfenetre)
                (let ((partiel (* transpo (nth k (first (second analyse))))))
                  (if (< partiel limite) 
                    (progn
                      (format file "i~A" instrument)
                      (format file "~8,3F" datedeb )
                      (format file "~8,3F" dur0)
                      (format file "~9,4F" (* mulampcorr (nth k (first (third analyse))))) ; amps
                      (format file "~9,2F" partiel)  ; freqs  
                      (format file "~8,3F" pente0 )
                      (format file "~8,2F" (nth k coefspa) )
                      (princ #\newline  file)  )))))
                  
            
              ;-------  suite des échantillons  ----------
            
            (for  (i 1 1 nbfen)
              (if (= (mod i 10) 0) (format t " ~A "  i))
              
              (let* ((longfenetre (1- (length (nth i (second analyse)))))
                     (dati  (nth i datanal))
                     (pentedroite (* multuile (- (nth (1+ i) datanal) dati)))
                     (pentegauche (* multuile (- dati (nth  (1- i) datanal))))
                     (duri (+ pentedroite pentegauche)) 
                     (coefspa (poly-spatia i (nth i (second analyse)) spatia mode 
                                           (if (atom minspa) minspa (nth n minspa))
                                           (if (atom minspa) maxspa (nth n maxspa)))))
                
                (for (k 0 1 longfenetre)
                  (let ((partiel (* transpo (nth k (nth i (second analyse))))))
                    (if (< partiel limite)
                      (progn
                        (format file "i2")
                        (format file "~8,3F" (+ offn (* muldur dati (+ 1 (/ (om-random (- alea) 
                                                                                         (float alea)) 1000)) ))) ; date   
                        (format file "~8,3F" duri)   ; durée
                        (format file "~9,4F" (* mulamp (nth k (nth i (third analyse)) ))) ; amps
                        (format file "~9,2F" partiel) ; freqs  
                        (format file "~8,3F" pentedroite)  ; pente
                        (format file "~8,2F" (nth k coefspa) )
                        (princ #\newline  file) ))) )   ))  )
              
              (princ #\newline  file)  )
            
        (princ #\e  file)))
        
     (format t  "~% fichier ~A écrit  - durée ~8,3F ~%" nom fin))
      nil)





; -------------------------------gran-st-sco-tab--------------------------------------
; essai utilisation de tableaux

(defun ll->tab (liste larg)
  (let ((liste (complete-lliste liste larg nil)) (dim (list (length liste) larg)))
    (make-array dim :initial-contents liste)))

;A Verifier
(om::defmethod! gran-st-sco-tab ((dates list) 
                                    (tuile number)
                                    (freqs  list) 
                                    (amps  list)
                                    (alea number)
                                    (spatia  list)
                                    (fichier list)) 

   :initvals (list '(1 2) 1 '(1 2) '(1 2) 0 '(1 2) nil)
   :indoc '("dates" "tuile" "frqs" "amps" "alea" "spatia" "fichier")
   :icon 132
   :doc ""

  (let* ((nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier))
        (long (1- (length dates)))
        (larg (list-max (mapcar 'length freqs)))
        (last-ev (- (nth long dates) (nth (1- long) dates )))
        (dates (x-append dates (+ (last-elem dates ) last-ev)  (+ (last-elem dates ) 
                                                                  (* 2 last-ev))))

        (tdates (make-array (+ 3 long)  :initial-contents dates))
        (tfreqs (ll->tab freqs larg))
        (tamps (ll->tab amps larg))
        (tspatia (ll->tab spatia larg)))

    (when nom
      (with-open-file (file nom :direction :output 
                            :if-exists :supersede)
    
        (format file "f1 0 1024 10 1 ~%~%")

        (for  (i 0 1 long)
          (for (k 0 1 (1- larg))
            (if (not (null (aref tfreqs i k)))
              (progn
                (format file "i1" )
                (format file "~8,3F" (* (aref tdates i) (+ 1 (/ (om-random (- alea) 
                                                                             (float alea)) 1000)) ))   ; dates   
                (format file "~8,3F" (* tuile (- (aref  tdates (+ 2 i)) (aref tdates i))) )  ; durées  
                (format file "~9,4F" (aref tamps  i k))   ; amps
                (format file "~9,2F" (aref tfreqs i k))   ; freqs  
                (format file "~8,3F" (* tuile (- (aref tdates (+ 2 i)) (aref tdates (1+ i))))) ; pentes
                (format file "~8,2F" (aref tspatia i k) file)
                (princ #\newline  file)  ))))

        (princ #\e  file)))

    (format t  "fichier ~A écrit ~%" nom))
    nil)


; ================================================================================================




; ----------------------------------------------------------------------------------------------

(om::defmethod! format-suivi ((l-dates  list) 
                                 (l-freqs list)
                                 (l-amps list) 
                                 (tuilage number)
                                 (extinct number)
                                 (spectre list))

   :initvals (list '(1 2) '(1 2) '(1 2) 1 1 '((1) (1)))
   :indoc '("l-dates" "l-freqs" "l-amps" "tuilage" "extinct" "spectre")
   :icon 132
   :doc "crée fichier pour text-win ; score pour instrument SUIVI "



  (let* ((nbcomp (1- (length (first l-freqs))) )      ;  nb partiels
         (nbechant (1- (length l-dates)))
         (l-durs (om+ (x-append (x->dx l-dates) extinct)  tuilage))
         (l-freqs (x-append l-freqs (list (last-elem l-freqs) )))
         (zeros (list (create-list (1+ nbcomp) 0)))
         (l-amps  (x-append l-amps zeros  ))
         (spectre (list! spectre))
         res)
    
    (push (format () "f1 0 1024 10 ~{ ~4F~} ~% ~%" spectre) res)
    
    
    ;(push #\newline res)

    (for  (i  0  1  nbechant)
      (for (k  0  1  nbcomp)
        (push
         (format () "i1 ~6F ~6F ~6F ~6F ~6F ~6F ~6F"  
                 (l-nth l-dates i)     (l-nth l-durs i)
                 (l-nth (l-nth l-amps  i) k)   (l-nth (l-nth l-amps  (1+ i)) k)
                 (l-nth (l-nth l-freqs i) k)   (l-nth (l-nth l-freqs (1+ i)) k)
                 tuilage)   res)
        (push #\newline res))
      (push #\newline res))

    (push #\e res)

    (nreverse res)))


(om::defmethod! write-suivi ((l-dates  list) 
                                (l-freqs list)
                                (l-amps list) 
                                (tuilage number)
                                (extinct number)
                                (spectre list)
                                (fichier list))
   :initvals (list '(1 2) '(1 2) '(1 2) 1 1 '((1) (1)) nil)
   :indoc '("l-dates" "l-freqs" "l-amps" "tuilage" "extinct" "spectre" "fichier")
   :icon 132
   :doc "crée fichier sur disque ; score pour instrument SUIVI "




   (let* ((nbcomp (1- (length (first l-freqs))) )      ;  nb partiels
          (nbechant (1- (length l-dates)))
          (l-durs (om+ (x-append (x->dx l-dates) extinct)  tuilage))
          (l-freqs (x-append l-freqs (list (last-elem l-freqs) )))
          (zeros (list (create-list (1+ nbcomp) 0)))
          (l-amps  (x-append l-amps zeros  ))
          (spectre (list! spectre))    
          (nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier)))

    (when nom
      (with-open-file (file nom :direction :output   :if-exists :supersede)
    
    (format file "f1 0 1024 10 ~{ ~4F~} ~% ~%" spectre)

    (for  (i  0  1  nbechant)
      (for (k  0  1  nbcomp)
        (format file "i1 ~6F ~6F ~6F ~6F ~6F ~6F ~6F  ~%"  
                 (l-nth l-dates i)     (l-nth l-durs i)
                 (l-nth (l-nth l-amps  i) k)   (l-nth (l-nth l-amps  (1+ i)) k)
                 (l-nth (l-nth l-freqs i) k)   (l-nth (l-nth l-freqs (1+ i)) k)
                 tuilage))
      (princ #\newline  file))
      
    (princ #\e file))))
  (print  "fichier  écrit   "))


; ------ tentative de créer des suivis de partiels à partir d'une liste d'accords qcq ---




(om::defmethod! wr-cree-suivi ((l-dates  list) 
                                  (l-midics list)
                                  (l-amps list)
                                  (toler number)
                                  (tuilage number)
                                  (extinct number)
                                  (spectre t)       ; normalement list!
                                  (fichier list))
   
   :initvals (list '(1 2) '(1 2) '(1 2) 25 0.05 1.0 1 nil)
   :indoc '("l-dates" "l-midics" "l-amps" "tolerance" "tuilage" "extinct" "spectre" "fichier")
   :icon 132
   :doc "crée fichier sur disque ; score pour instrument SUIVI
crée un pseudo suivi de partiels à partir d'une liste d'accords qcq
<toler> (cents) indique la déviation en freq  en + ou - acceptée  pour chaque partiel
ATTENTION : entrée en Midics "
   
   
   (let* ((nbechant (1- (length l-dates)))
          (l-durs (om+ (x-append (x->dx l-dates) extinct)  (* 2 tuilage)))
          (l-midics (x-append l-midics (list (last-elem l-midics) )))   ; création données
          (zeros (list (create-list (length (last-elem l-midics)) 0)))  ; midi et amp pour
          (l-amps  (x-append l-amps zeros  ))                           ; extinction
          (resul (suivi-partiels l-midics l-amps nbechant toler))
          (l-freqs-fin (first resul))
          (l-amps-fin  (second resul))
          (spectre (list! spectre))
          (nom (if (equal fichier 'nil) (choose-new-file-dialog ) fichier)) )
     
     (when nom
       (with-open-file (file nom :direction :output   :if-exists :supersede)
         
         (format file "f1 0 1024 10 ~{ ~4F~} ~% ~%" spectre)
         
         (for  (i  0  1  nbechant)
           (for (k  0  1  (1- (length (l-nth l-midics i))))
             (format file "i1 ~6F ~6F ~6F ~6F ~6F ~6F ~6F  ~%"  
                     (l-nth l-dates i)     (l-nth l-durs i)
                     (l-nth (l-nth l-amps  i) k)   (l-nth (l-nth l-amps-fin i) k)
                     (l-nth (l-nth (mc->f l-midics) i) k)   (l-nth (l-nth l-freqs-fin i) k)
                     tuilage))
           (princ #\newline  file))
         
         (princ #\e file))))
   
   (print  "fichier  écrit   "))



; ------2e tentative de créer des suivis de partiels à partir d'une liste d'accords qcq ---



(om::defmethod! cree-suivi ((l-dates  list)  
                               (l-midics list)
                               (l-amps list)
                               (toler number)
                               (fade number) 
                               (tuilage number)
                               (attaque number)
                               (extinct number)
                               (spectre t))

   :initvals (list '(1 2) '(1 2) '(1 2) 25 .2 .05 0 1.0 1)
   :indoc '("l-dates" "l-midics" "l-amps" "tolerance" "fade" "tuilage" "attaque" "extinct" "spectre")
   :icon 132
   :doc "crée fichier pour text-win ; score pour instrument SUIVI
crée un pseudo suivi de partiels à partir d'une liste d'accords qcq
<toler> (cents) indique la déviation en freq  en + ou - acceptée  pour chaque partiel
<fade>  durée d'extinction des partiels non suivis ou d'apparition des partiels 
nouveaux (pourcentage de la durée de l'échantillon de gauche)
<tuilage> tuilage des partiels suivis (durée fixe en sec)
<extinct> durée d'extinction du dernier échantillon (sec)
ATTENTION : entrée en Midics "
  
        
  
  (let* ((nbechant (1- (length l-dates)))
         (l-durs  (x-append (x->dx l-dates) extinct)  )
         (l-dates (om+ l-dates attaque))
         (l-midics (x-append l-midics (list (last-elem l-midics) )))   ; création données
         (zeros (list (create-list (length (last-elem l-midics)) 0)))  ; midi et amp pour
         (l-amps  (x-append l-amps zeros  ))                           ; extinction
         (toler (* 2 toler))
         (spectre (list! spectre))
         res)
    
    (push (format () "f1 0 1024 10 ~{ ~4F~} ~% ~%" spectre) res)

;                                      -------    premier accord    ---------
    (let ((accord (first l-midics ))
          (prochacc (second l-midics ))
          (ampli (first l-amps)) (prochampli (second l-amps ))
          (fadefin (* (first l-durs) fade)))
          
          (for (k  0  1  (1- (length accord)))    ;----  boucle des notes  ---------
            (let* ((resul (suivi? (l-nth accord k) (l-nth ampli k) prochacc prochampli toler))
                   (suiv (first resul ))
                   (dur  (+ (if suiv (+ (first l-durs ) tuilage) (first l-durs )) attaque))
                   (tuilfin (if suiv tuilage fadefin ))
                   (notefin (second resul))
                   (ampfin (third resul)))
              
              (push  (format () "i1 ~6F ~6F ~6F ~6F ~6F ~6F ~6F ~6F ~%" 
                       0    dur
                       (l-nth ampli k)   ampfin
                       (mc->f (l-nth accord k) )  (mc->f notefin)
                       attaque tuilfin)   res) )))
   (push #\newline res)
    
    (for  (i  1  1  nbechant)      ;===============  boucle des accords ===================
      (let ((accord (l-nth l-midics i)) (precedacc (l-nth l-midics (1- i)))
            (prochacc (l-nth l-midics (1+ i)))
            (ampli (l-nth l-amps i)) (prochampli (l-nth l-amps (1+ i)))
            (fadedeb (* (l-nth l-durs (1- i)) fade))
            (fadefin (* (l-nth l-durs  i) fade)))
        
        (for (k  0  1  (1- (length accord)))    ;----  boucle des notes  ---------
          (let* ((resul (suivi? (l-nth accord k) (l-nth ampli k) prochacc prochampli toler))
                 (suiv (first resul ))
                 (nouv (nouvel? (l-nth accord k) precedacc toler))
                 (date (if nouv (- (l-nth l-dates i) fadedeb) (l-nth l-dates i)))
                 (dur  (if suiv (+ (l-nth l-durs i) tuilage) (l-nth l-durs i)))
                 (dur  (if nouv (+ dur fadedeb) dur))
                 (tuildeb (if nouv fadedeb tuilage))
                 (tuilfin (if suiv tuilage fadefin ))
                 (notefin (second resul))
                 (ampfin (third resul)))
            
            (push
             (format () "i1 ~6F ~6F ~6F ~6F ~6F ~6F ~6F ~6F~%" 
                     date    dur
                     (l-nth ampli k)   ampfin
                     (mc->f (l-nth accord k) )  (mc->f notefin)
                     tuildeb tuilfin)   res) )
            )
        )                         ;--------------------------------------------------
      (push #\newline res))       ; =========================================================

    (push #\e res)
    (nreverse res)))




(defun suivi? (note amp prochacc prochampli toler)
  
  (let* ((ecarts (om-abs (om- prochacc note)))
         (ecartmin (list-min ecarts))
         (numcible (position ecartmin ecarts))
         (suivi? (< ecartmin toler))
         (notefin (if suivi? (l-nth prochacc numcible) note))
         (ampfin  (if suivi? (l-nth prochampli numcible) amp)))
    (list suivi? notefin ampfin)))


(defun nouvel? (note precedacc toler)
  
  (let ((ecartmin (list-min (om-abs (om- precedacc note)))))
    (> ecartmin toler)))
    
    




; lecture d'un fichier d'analyse .chseq  (markers)

(om::defmethod! as-markers ((analyse list)
                               (fqmin number)
                               (fqmax number) 
                               (*amp number))

   :initvals (list '(1 2) 60 440 .5)
   :indoc '("analyse" "fqmin" "fqmax" "*amp")
   :icon 132
   :doc "convertit fichier d'analyse type markers-chseq
sortie : liste ((dates ) ((listes fq)) ((listes amps)))
les amps sont mises à l'échelle - somme des amps=1 " 



  (let* ((analyse (cddr (car analyse)) )
         (dates (mapcar 'third analyse))
         (fqs (mapcar 'fourth analyse))
         (amps (mapcar 'fifth analyse))

         (res (band-multi-filter  (list dates fqs amps) 1 fqmin fqmax  1))

         (dates (om- (car res) (car (car res))))
         (datesdiff   (unique-1 dates '= ) )
         
         (lgdate (x->dx (x-append (car-mapcar 'position datesdiff dates) (length dates))))

         ;(amps (om-scale (group-list (third res) lgdate 'linear)  0.001 1))
         ;(ampmax (list-max (l-sum  amps)))
         ;(amps (om-round (om* (om/ amps ampmax) *amp) 4)))

         (amps (om-scale (third res) 0.00 1))
         (amps (om*  amps (g-exp (om* 2 amps)))))
    
    (list datesdiff (group-list (second res) lgdate 'linear) 
          (group-list amps lgdate 'linear))))
          
  
  




#|

A Carlos a faire, car celà touche l'interface


;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;; modif du format de sortie dans les text-windows  (pas de notation exponentielle)
; pas de guillemets écrits dans text-window quand on utilise (format .....)  
;modifications pour text-window ->> qlist (serge lemouton -- 7/06/93)
;cf file-buffer.lisp



(defpackage "C-PATCH-FILE-BUFFER"
  (:use "COMMON-LISP" "CCL")
  (:import-from "PATCH-WORK"
                "*TARGET-ACTION-OBJECT*" "NEW-MENU" "NEW-LEAFMENU" "MAKE-POPUPBOX"
                "PATCH-VALUE" "H" "W" "C-PATCH" "PW-CONTROLS" "INPUT-OBJECTS" "LIST!"
                "PW-FUNCTION-STRING" "DEFUNP" "COMPLETE-BOX" "REMOVE-YOURSELF-CONTROL")
  (:export "C-PATCH-FILE-BUFFER"))

(in-package "C-PATCH-FILE-BUFFER")


(defvar *lisp-win-option* t)


(defclass C-patch-ascii-buffer (C-patch-file-buffer) ())




(defmethod add-to-file ((self C-patch-ascii-buffer) list format)
  (if *lisp-win-option*
   ; (call-next-method)
   (let* ((list (list! list))
           (count 0)
           (format (if (zerop format) (length list) format))
           (mark (fred-buffer
                  (if (not (and (fred-win self) (wptr (fred-win self))))
                    (get-new self) (fred-win self)))))
      (dolist (item list)
        (buffer-insert mark (format nil " ~8F" item));modif serge
        (if (zerop (rem (incf count) format))
          (buffer-insert mark (format nil ";~A" #\Return)))))

    (let* ((list (list! list))
           (count 0)
           (format (if (zerop format) (length list) format))
           (mark (fred-buffer
                  (if (not (and (fred-win self) (wptr (fred-win self))))
                    (get-new self) (fred-win self)))))
      (dolist (item list)
        (buffer-insert mark (format nil " ~A" item));modif serge
        ;(buffer-insert mark (format nil " ~8F" item));modif serge
        (if (zerop (rem (incf count) format))
          (buffer-insert mark (format nil "~%"))))))
  )
|#



