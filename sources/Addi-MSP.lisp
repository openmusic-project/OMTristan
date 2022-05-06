(in-package :om)

; ----------------   librairie pour Additive (Diphone) -> Max MSP  ------------------

; Migth do it in Sdiff lib!!??

(defun addicoll (add-file max-file )
"traduit les fichiers additive dans le format des coll MSP"
(let* ((file-in (if (equal add-file 'nil) (om-choose-file-dialog ) add-file))
      (file-out (if (equal max-file 'nil) (om-choose-new-file-dialog ) max-file))
      (window  (make-array  163  :adjustable t  :fill-pointer 0))
      item  )
         
         (when file-in
           (with-open-file (file file-in :direction :input 
                                 :if-does-not-exist nil)
             (with-open-file (file2 file-out :direction :output 
                                   :if-exists :supersede)

               ; lecture de la 1ere fenêtre
               (do   ((n 1 (+ 1 n))) ((= n 163) nil)
                   (setq item (read file nil :eof))
                   (setf  (aref  window n) item ))

               ; le 1ere fenêtre est écrite deux fois (pour la rampe d'attaque)
               (for (j 0 1 1)
                 (print j)
                 (princ j file2)
                 (princ #\, file2)
                 (for (m 4 4 162 )
                   (format file2 "~9,2F" (aref window m))
                   (format file2 "~9,4F" (aref window (+ 1 m))))
                 (princ #\; file2)
                 (princ #\newline file2))


               ; boucle de lecture des fenêtres suivantes
               (do  ((k 2 (+ 1 k))) ((eq item :eof) nil)
                 
                 (do   ((n 1 (+ 1 n))) ((= n 163) nil)
                   (setq item (read file nil :eof))
                   (setf  (aref  window n) item ))

                 (if (= 0 (mod k 10)) (print k))
                 (princ k file2)
                 (princ #\, file2)
                 
                 (if (eq item :eof) 
                   (princ "   end" file2)
                   (for (m 4 4 162 )
                     (format file2 "~9,2F" (aref window m))
                     (format file2 "~9,4F" (aref window (+ 1 m)))))
                   
                 (princ #\; file2)
                 (princ #\newline file2))  
               )))
         ))



(om::defmethod! addi->coll ((add-file symbol) 
                               (max-file symbol))

   :initvals (list 'nil 'nil)
   :indoc '("add-file" "max-file")
   :icon 158
   :doc "traduit les fichiers Additive dans le format des coll MSP
ext: fichier = donner noms de fichier avec leur ''path'' - sinon
un dialogue s'ouvre"

        
  (addicoll add-file max-file))



(om::defmethod! addi->tfa ((add-file symbol) (dim number))

   :initvals (list 'nil 2000)
   :indoc '("add-file" "max-file")
   :icon 158
   :doc "lit les fichiers additive et en extrait des tableaux de freq et d'amp "

 
       (let* ((file-in (if (equal add-file 'nil) (om-choose-file-dialog ) add-file))
             (dimension (list 40 dim))
             (t-freq  (make-array  dimension  :adjustable t  ))
             (t-amp   (make-array  dimension  :adjustable t  ))
              newdim kfen nbpar)
         
         (when file-in
           (with-open-file (file file-in :direction :input 
                                 :if-does-not-exist nil)
             
             ; boucle de lecture des fenêtres
             (do  ((k 0 (+ 1 k))) ((eq nbpar :eof) nil)  
               (setq nbpar (read file nil :eof))  ; nb partiels est lu
               
               (if (eq nbpar :eof)
                 ()
                 (progn

                   (if (= 0 (mod k 10)) (print k))
                   ;(print k)
                   (read file )    ; time est lu
               
                   (for (rang 0 1 39)
                     (read file)    ; nb rang est lu
                     (setf (aref t-freq rang k) (read file))
                     (setf (aref t-amp  rang k) (read file))
                     (read file)    ; phase est lue
                     (setq kfen k))
                    )
                 )
               )
             ))

(setq newdim (list 40 kfen))
(print (format nil "nb fenêtres: ~A " (1+ kfen)))
(list (adjust-array t-freq newdim) (adjust-array t-amp newdim))
))


;  en chantier---------------

(om::defmethod! addi->lfa ((add-file symbol))
   :initvals (list 'nil 2000)
   :indoc '("add-file" "max-file")
   :icon 158
   :doc "lit les fichiers additive et en extrait des listes de freq et d'amp"

 
       (let* ((file-in (if (equal add-file 'nil) (om-choose-file-dialog ) add-file))
             l-freq l-amp kfen nbpar )
         
         (when file-in
           (with-open-file (file file-in :direction :input 
                                 :if-does-not-exist nil)
             
             ; boucle de lecture des fenêtres
             (do  ((k 0 (+ 1 k))) ((eq nbpar :eof) nil)  
               (setq nbpar (read file nil :eof))  ; nb partiels est lu
               (let (fqlist amplist )
                 (if (eq nbpar :eof)
                   ()
                   (progn
                     (if (= 0 (mod k 10)) (print k))
                     ;(print k)
                     (read file )    ; time est lu
                     
                     (for (rang 0 1 39)
                       (read file)    ; nb rang est lu
                       (push (read file) fqlist)
                       (push (read file) amplist)
                       (read file))    ; phase est lue
                     (setq kfen k))
                   )
                 (push (reverse fqlist) l-freq)
                 
                 (push (reverse amplist) l-amp))
               )))

(print (format nil "nb fenêtres: ~A " (1+ kfen)))
(list (butlast (nreverse l-freq)) (butlast (nreverse l-amp )))  ; butlast filtre un 'nil' final
))


; -----------------------------------

  
  
(om::defmethod! lfa->coll ((l-freqs list) (l-amps list) (file-out string))

   :initvals (list '(1 2) '(1 2) "name")
   :indoc '("l-freqs" "l-amps" "file-out" )
   :icon 158
   :doc "écrit un fichier au format coll pour Max MSP à partir de listes de freq et d'amp"

        
  (let ((nharm (1- (length l-freqs))) 
        (nbfen  (- (length (first l-freqs)) 1))
        (l-freqs (om-round l-freqs 2))
        (file-out (if (equal file-out "name") (om-choose-new-file-dialog ) file-out)))
    
    (with-open-file (file2 file-out :direction :output 
                           :if-exists :supersede)

      ; la 1ere fenêtre est écrite deux fois (pour la rampe d'attaque)
      (princ 0 file2)
      (princ #\, file2)
      (for (h 0 1 nharm)
        (format file2 "~9,2F" (l-nth (l-nth l-freqs h) 0))
        (format file2 "~9,4F" (l-nth (l-nth l-amps  h) 0)))
      (princ #\; file2)
      (princ #\newline file2)
      
      (for (f 0 1 nbfen)
        (if (= 0 (mod f 10)) (print f))
        (princ (+ 1 f) file2)
        (princ #\, file2)
        
        (for (h 0 1 nharm)
          (format file2 "~9,2F" (l-nth (l-nth l-freqs h) f))
          (format file2 "~9,4F" (l-nth (l-nth l-amps  h) f)))
        
        (princ #\; file2)
        (princ #\newline file2)) 
      
      (princ (+ 2 nbfen) file2)
      (princ #\, file2)
      (princ "   end" file2)
      (princ #\; file2)
      
      (format t "~%nb fenêtres: ~A ~%" (+ 1 nbfen))
      nil
      )))



         
