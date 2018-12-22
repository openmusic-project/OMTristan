(in-package :om)

; ------------------- max-next --------------------

(om::defmethod! extract-freq ((fichier list)  (partiels list))

   :initvals (list '(1 2) '(1 2))
   :indoc '("fichier" "partiels")
   :icon 164
   :doc "extrait les fréquences d'une q-list" 


  (let  ((long (length fichier)) (partiels (carlist! partiels)) res)
    (if (atom partiels)
      (setq res (extract-one-freq fichier long partiels))
      (progn (dolist (n partiels) (push (extract-one-freq fichier long n) res)) ))
    (if (atom partiels) res (nreverse res))))
  


(om::defmethod! extract-amp ((fichier list)  (partiels list))

   :initvals (list '(1 2) '(1 2))
   :indoc '("fichier" "partiels")
   :icon 164
   :doc "extrait les amplitudes d'une q-list" 

  (let  ((long (length fichier)) (partiels (carlist! partiels)) res)
    (if (atom partiels)
      (setq res (extract-one-amp fichier long partiels))
      (progn (dolist (n partiels) (push (extract-one-amp fichier long n) res)) ))
    (if (atom partiels) res (nreverse res))))
  
  

(defun extract-one-freq (fichier long partiel)
  (l-nth fichier (arithm-ser (+ (* 2 partiel) (* 2 (truncate (1- partiel) 5 ))) long 96 )))

(defun extract-one-amp (fichier long partiel)
  (l-nth fichier (arithm-ser (1+ (+ (* 2 partiel) (* 2 (truncate (1- partiel) 5 )))) long 96)))


(defun extract-one-freq-t (tab long partiel)
  (l-aref tab (arithm-ser (+ (* 2 partiel) 
                                      (* 2 (truncate (1- partiel) 5 ))) long 96 )))

(defun extract-one-amp-t (tab long partiel)
  (l-aref tab (arithm-ser (1+ (+ (* 2 partiel)
                                         (* 2 (truncate (1- partiel) 5 )))) long 96 )))





(om::defmethod! ql->freqs ((fichier list))

   :initvals (list '(1 2))
   :indoc '("fichier" )
   :icon 164
   :doc "extrait les freq de tous les partiels d'une q-list " 


  (let  ((long (length fichier))  res)
    (for (n 1 1 40)
      (print n)
      (push (extract-one-freq fichier long n) res))
    (nreverse res) ))

(om::defmethod! ql->amps ((fichier list))
   :initvals (list '(1 2))
   :indoc '("fichier")
   :icon 164
   :doc "extrait les amp de tous les partiels d'une q-list
((amp-partiel1)  (amp-partiel2)... ) " 

        
  (let  ((long (length fichier))  res)
    (for (n 1 1 40)
      (print n)
      (push (extract-one-amp fichier long n) res)
      )
    (nreverse res)))





(om::defmethod! qlt->fqs ((tab t))
   :initvals (list t)
   :indoc '("tab")
   :icon 164
   :doc "extrait les freq de tous les partiels d'une q-list lue
sous forme de tableau : ((fq-partiel1) (fq-partiel2)... )"
  (let  ((long (length tab))  res)
    (for (n 1 1 40)
      (print n)
      (push (extract-one-freq-t tab long n) res))
    (nreverse res) ))


(om::defmethod! qlt->ams ((tab t))

   :initvals (list t)
   :indoc '("tab")
   :icon 164
   :doc "extrait les amp de tous les partiels d'une q-list
((amp-partiel1)  (amp-partiel2)... ) " 
        
  (let  ((long (length tab))  res)
    (for (n 1 1 40)
      (print n)
      (push (extract-one-amp-t tab long n) res))
    (nreverse res)))



; corrigé 14/4/2000   7/6/2003

(om::defmethod! analyse-spectre ((freqs list) (amps list) 
                                    (deb number) (fin number)
                                    (nharm number))
   :initvals (list '(1 2) '(1 2) 0 1 1)
   :indoc '("freqs" "amps" "deb" "fin" "nharm")
   :icon 133
   :doc "calcule un spectre moyen d'après les <nharm> premières harmoniques
d'une analyse dynamique freqs-amps au format suivi de partiels
(liste (freqs partiel 1) (freqs partiel 2)...)
Calcule la moyenne sur les fenêtres situées entre deb et fin
Rend une double liste ((freqs) (amps))" 

  (let  ((freqs (l-nth  freqs (arithm-ser 0 (1- nharm) 1 )))
         ( amps (l-nth amps (arithm-ser 0 (1- nharm) 1 )))
         ana-freqs ana-amps)
   
    (dolist  (f freqs)
      (push (filtre-liste  '= 0  (list-pos f  deb  fin)) ana-freqs))

    (dolist (a amps)
      (push (list-pos  a  deb  fin) ana-amps))

    (list (tm-average (nreverse ana-freqs) 1) (tm-average (nreverse ana-amps) 1))))

; ancienne version
(om::defmethod! spectre-moyen/a ((freqs list) (amps list) 
                                    (deb number) (fin number)
                                    (nharm number) (coupure number)
                                    &optional (minvel 10) (maxvel 127))
   :initvals (list '(1 2) '(1 2) 0 10 20 50 10 127)
   :indoc '("freqs" "amps" "deb" "fin" "nharm" "filtre freq" "vel min" "vel max")
   :icon 133
   :doc "calcule un spectre d'après les <nharm> premières harmoniques
d'une analyse dynamique par fenêtre  - par ex. masking effects.
(liste (freqs fenetre 1) (freqs fenetre 2)...)
(on obtient le format suivi de partiels utilisé dans analyse-spectre par une transposition de matrice)
Ne marche que pour son assez stable.
Calcule la moyenne sur les fenêtres situées entre deb et fin
Filtre frequences basses <coupure> en hz
Rend objet accord (on peut ajuster ambitus des vélocités <minvel> <maxvel> )" 

(let* ((spfil 
        (loop for f in freqs
              for a in amps
              collect (multi-filter  '<  coupure (list f a) 0)))

       (spanalys (analyse-spectre (mat-trans (mapcar 'first spfil) )
                                  (mat-trans (mapcar 'second spfil)) deb fin nharm)))


  (mki 'chord
       :LMidic (f->mc (first spanalys))
       :Lvel (om-round (om-scale (tm-lin->db (om-scale/max  (second spanalys) 1) 8) minvel maxvel))
       :Loffset '(0)
       :Ldur '(1000)
       :Lchan '(1))))


; nv version oct 2004

(om::defmethod! spectre-moyen ((freqs list) (amps list) 
                                    (deb number) (fin number)
                                    (nharm number) (coupure number)
                                    &optional (minvel 10) (maxvel 127))
   :initvals (list '(1 2) '(1 2) 0 10 20 50 10 127)
   :indoc '("freqs" "amps" "deb" "fin" "nharm" "filtre freq" "vel min" "vel max")
   :icon 133
   :doc "calcule un spectre d'après les <nharm> premières harmoniques
d'une analyse dynamique par fenêtre  - par ex. masking effects.
(liste (freqs fenetre 1) (freqs fenetre 2)...)
(on obtient le format suivi de partiels utilisé dans analyse-spectre par une transposition de matrice)
Ne marche que pour son assez stable.
Calcule la moyenne sur les fenêtres situées entre deb et fin
Filtre frequences basses <coupure> en hz
Rend objet accord (on peut ajuster ambitus des vélocités <minvel> <maxvel> )" 

(let* ((freqs (list-pos freqs deb fin))
       (amps (list-pos amps deb fin))
       (nharm (min nharm (l-min (mapcar 'length freqs))))
       (spfil 
        (loop for f in freqs
              for a in amps
              collect (multi-filter  '<  coupure (list f a) 0)))

       (spanalys (analyse-spectre (mat-trans (mapcar 'first spfil) )
                                  (mat-trans (mapcar 'second spfil)) 0 (- (length freqs) 1) nharm)))


  (mki 'chord
       :LMidic (f->mc (first spanalys))
       :Lvel (om-round (om-scale (tm-lin->db (om-scale/max  (second spanalys) 1) 8) minvel maxvel))
       :Loffset '(0)
       :Ldur '(1000)
       :Lchan '(1))))











(om::defmethod! melo->env ((rangs list) 
                              (durs list)
                              (nharm number)
                              (amp number)
                              (nbsamp number))

   :initvals (list '(1 2) '(1 2) 1 1 1)
   :indoc '("rangs" "durs" "nharm" "amp" "nbsamp")
   :icon 133
   :doc "crée une enveloppe où amp sera au max à 
chaque fois qu'on trouvera l'harm <nharm> dans la 
mélodie rangs <rangs>, et aussi longtemps que la durée
correspondante de <durs>
nbsamp = nb de points de l'enveloppe" 

  (let ((durs2 (om-round  (om-scale/sum durs nbsamp))) lval)
    (dolist (i rangs)
      (push (if (= i nharm) amp 0) lval))
    (flat  (mapcar 'create-list durs2 (nreverse lval)))))




(om::defmethod! melo->env+pond ((rangs list) 
                                   (durs list)
                                   (nharm number)
                                   (env2 list)
                                   (pond% number))

   :initvals (list '(1 2) '(1 2) 1 '(1 2) 1.0)
   :indoc '("rangs" "durs" "nharm" "env2" "pond%")
   :icon 133
   :doc "crée une enveloppe où amp sera au max à 
chaque fois qu'on trouvera l'harm <nharm> dans la 
mélodie rangs <rangs>, et aussi longtemps que la durée
correspondante de <durs>, le tout étant pondéré par l'env 2, 
selon <pond%>, exprimé en pcentage
le nb de points de l'env et son amp max sont sont de env2" 


  (let* ((env2 (flat env2)) (amp2 (list-max env2))
         
         (env1 (om-scale/max (om+ (melo->env rangs durs nharm amp2 (length env2))
                                  (om/ (om* env2 pond%) 100)) amp2)))

    (setq env1 (cond   (  (> (length env1) (length env2)) (butlast env1))
            (  (< (length env1) (length env2)) (x-append env1 0))
            (t env1))))) 





(om::defmethod! env-globale  ((l-amps list) 
                                 (envglo list)
                                 (type symbol)
                                 (sens symbol))
   
   :initvals (list '(1 2) '(1 2) 'abrupt 'normal )
   :indoc '("l-amps" "envglo" "type" "sens")
   :menuins '((2 (("abrupt" 'pitch)
                 ("pentes" 'rotor)))
             (3 (("normal" 'midic)
                 ("renv" 'frq))))
   :icon 133
   :doc " sculpte un son selon l'enveloppe globale envglo ; les env de
chaque partiel sont éventuellement ramenées à 0 ,pour créer
les contours d'attaque et d'extinction (du grave à l'aigu)
envglo doit avoir exactement la même longueur que chaque
liste d'amplitudes
options: 
abrupt : les partiels apparaissent ''brusquement'' , en
fct des contours d'envglo
pentes : ils apparaissent doucement, depuis le début du son,
et atteignent leur max, en fct d'envglo
normal : les hq se déploient du grave à l'aigu, selon envglo
renv : ils se déploient de l'aigu au grave comme si envglo
était renversée" 
   
   (let* ((envglo (om-scale/max envglo 40))
          (type (case type
                  (abrupt 1)
                  (pentes 2)))
          (sens (case sens
                  (normal 1)
                  (renv 2)))
          res)
     (for (n 0 1 39)
       (push (env-un-partiel envglo n type sens) res))
     (om* (nreverse res) l-amps)))
  

(defun env-un-partiel (envglo nharm type sens)
  (let ((env (if (= sens 1) (om-  envglo nharm) (om-  envglo (- 39 nharm)))))
    (if (= type 1 ) (mapcar 'test-zero-un env)
        (if (< (list-min env) 0) (om-scale (mapcar 'test-un env) 0 1 ) (mapcar 'test-un env) ))))
        

(defun test-zero-un (val)
  (cond (  (< val 0) 0)
        (  (> val 1) 1)
        ( t  val)))

(defun test-un (val)
   (if  (> val 1) 1 val))
       



(om::defmethod! format-ql ((l-freqs list) (l-amps list))
   
   
   :initvals (list '(1 2) '(1 2) 'abrupt 'normal )
   :indoc '("l-freqs" "l-amps")   
   :icon 133
   :doc ""
   
   (let ((nharm (1- (length l-freqs))) (nbfen (1- (length (first l-freqs))))
         (l-freqs (om-round l-freqs 2)) res)
     
     (for (h 0 1 nharm)
       (if (= 0 (mod h 5)) (push (format () "0 i~A" (1+ h)) res))
       (push (l-nth (l-nth l-freqs h) 0) res)
       (push 0 res)
       (if (or (= h nharm) (= 4 (mod h 5)))
         (progn (push #\;  res) (push #\newline  res))))
     
     (for (f 0 1 nbfen)
       (for (h 0 1 nharm)
         (if (= 0 (mod h 5)) (push (format () "0 i~A" (1+ h)) res))
         (push (l-nth (l-nth l-freqs h) f) res)
         (push (l-nth (l-nth l-amps  h) f) res)
         (if (or (= h nharm) (= 4 (mod h 5))) 
           (progn (push #\;  res) (push #\newline  res)))))
     (nreverse res)))





;; ----------------------------------------------------------------------------------
;; modif du format de sortie dans les text-windows  (pas de notation exponentielle)
;modifications pour text-window ->> qlist (serge lemouton -- 7/06/93)
;cf file-buffer.lisp



#|
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
          (buffer-insert mark (format nil "~%")))))))

|#












#|
; ------------- sous-menu "Max-Next"

(in-package epw) 

(defparameter *max-menu* (new-menu "Max-Next"))

(ccl:add-menu-items patch-work::*pw-menu-patch*
    *max-menu* )

(PW-addmenu *max-menu* '(extract-freq extract-amp ql->freqs ql->amps
                         qlt->fqs qlt->ams analyse-spectre
                         melo->env melo->env+pond env-globale format-ql))

|#