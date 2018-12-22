;; ==================================================================================== 
;;                               OM-TRISTAN
;; ==================================================================================== 
        

(in-package :om)

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (file) (compile&load 
                        (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources")) 
                                       :name file)))
      '("OM-functions"
        "utils"
        "TMlibrairie-OM"
        "OM-CS"
        "Max-Next"
        "OMspdata"
        "Addi-MSP"
        "speartext"
        ))


(om::set-lib-release 3.4)


;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpack-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library
      '(("1-SPECTRAL HARMONY" (("HARMONIC SERIES" nil nil(sp-gen
                                                          n-sp-gen) nil)
                        
                        ("FREQUENCY MODULATION" nil nil (fmo
                                                         fm-origin
                                                         fm-ratio
                                                         fm-arp) nil)

                        ("AMPLITUDE MODULATION" nil nil (rmo
                                                         rm-gen
                                                         rm-approx
                                                         rm-intra
                                                         ch-rm-sine) nil)

                        ("DISTORTION" nil nil (disto
                                               dist-gen
                                               dist-sym
                                               dist-proc) nil)

                        ("FREQUENCY SHIFT" nil nil (fsh
                                                    fs-proc) nil)
                        ("VOCODER" nil nil (vocoder
                                            vocod-transp
                                            ch-vocoder
                                            time-vocoder) nil)

                        ("OTHER TREATMENTS" nil nil (f-interpol
                                                f-densifier
                                                reharmonizer
                                                diamanter
                                                ch-mixture
                                                f-multiplier
                                                proliferer) nil)

                        ("ANALYSIS" nil nil (virtual-fund
                                             virt-fund-step
                                             virt-fund-multi
                                             center-freq
                                             inter-freq
                                             which-harm
                                             closest-harm
                                             deviations
                                             which-dist
                                             match-n-sp
                                             match-dist
                                             match-trans) nil)

                        ("PROCESSING SPECTRAL ANALYSIS" nil nil (treat-ampl 
                                                                 midi-ampl) nil)) nil nil nil)

        ("2-INTERVALS" nil nil (closest-trans
                                oct-trans
                                chord-multiplier) nil)
                         
        ("3-LISTS" (("EXTRACT" nil nil (atom! 
                                        list-pos
                                        organizer
                                        penult) nil)
                    ("EDIT" nil nil (l-suppress 
                                     ll-suppress
                                     ll-remove
                                     ll-replace
                                     ll-insert
                                     guillotine
                                     substit) nil)
                    ("COMBINATORIAL" nil nil (stairs 
                                              sawtooth
                                              go-return
                                              spiral
                                              anaclasis
                                              combining
                                              grouping
                                              remove-dup%
                                              permut-rec
                                              permut-circ) nil)
                    ("OTHER TREATMENTS" nil nil (list!
                                                 densifier
                                                 smoothing
                                                 inverting
                                                 l-associate
                                                 l-simplify 
                                                 l-complete
                                                 create-matrix
                                                 sort-table) nil)
                    ("ANALYSIS" nil nil (l-sum 
                                         n-occur
                                         positions
                                         length-1) nil)) nil nil nil)
        ("4-OBJECTS" (("GENERATE" nil nil (trill
                                        triller
                                        arpeggio
                                        cresc-gen) nil)
                      ("EXTRACT" nil nil (select-filt
                                      nth-obj
                                      seq-extract) nil)
                   ("EDIT" nil nil (mixer
                                   chainer
                                   ch-onsets->seq
                                   chords->seq
                                   append-seq
                                   append-mseq
                                   add-chseq
                                   paste-object
                                   copy-paste
                                   paste-in-multi
                                   insert-object
                                   erase-chords
                                   insert-silence
                                   0start
                                   multi-seq-vide) nil)
                   ("TREATMENTS" nil nil (ch-modif
                                          lonset-modif
                                          lonset-modif-sel
                                           reverse-obj
                                           slur
                                           slur-stretch
                                           n-slur
                                           ch-distor
                                           ch-interpol
                                           ch-filter
                                           ch-test-filter
                                           ch-remdup
                                           velo
                                           tm-cresc
                                           tm-dur
                                           iso-dur
                                           canal
                                           newport
                                           portchan
                                           synch-fin
                                           ch-trim
                                           accel-ral
                                           stretch
                                           stretch-region
                                           stretch-chunk
                                           seq-stretch-curve
                                           mixtur) nil)
                   ("MAQUETTE" nil nil (order-maq
                                         sel-maq
                                         maq->mseq) nil)
                   ("ANALYSIS" nil nil (ch-length
                                       obj-dur
                                       obj-minmax
                                       canaux) nil)) nil nil nil)
        ("5-MATHEMATICS" (("SERIES" nil nil (arithm-crible
                                            n-arithm
                                            x-arithm
                                            fibonacci
                                            geom-ser
                                            triangle-ser
                                            puiss/to9-ser
                                            power-ser
                                            sinus-ser) nil)
                          ("SETS" nil nil (unique-notes
                                         common-notes
                                         notes-union
                                         notes-libres) nil)
                          ("FUNCTION ON X" nil nil (tree-oper
                                             sample-fun
                                             bpf-transfer
                                             linear-fct
                                             lagrange
                                             power/2
                                             power/3
                                             parabole2
                                             parabole3) nil)
                          ("FUNCTION ON LIST" nil nil (thales
                                              L*line
                                              L*curb
                                              l-distor/2
                                              l-distor/3
                                              deformer
                                              deformer%
                                              ll-deformer%
                                              minmax) nil)
                        ("CHAOS" nil nil (fractal+
                                          logistic
                                          henon) nil)
                        ("ALEATORIC" nil nil (LLalea
                                              tirage
                                              list-tirage
                                              list-alea-filter
                                              random-list
                                              random-from-list) nil)
                        ("ARITHMETIC" nil nil (tm-average
                                               om-modulo
                                               om-floor
                                               cumul
                                               diff
                                               l-pgcd
                                               ll-scaling
                                               l-scale%
                                               scale-r
                                               om-scale/max 
                                               <>
                                               ><
                                               x->dx+
                                               l-prime?
                                               accumule) nil)) nil nil nil)
        ("6-CONVERSIONS etc" (("LIST CONVERSIONS" nil nil (sec->min
                                                   min->sec
                                                   tm-lin->db
                                                   tm-db->lin
                                                   addtime
                                                   intertime
                                                   cumultime
                                                   pro-max
                                                   pro-max-dur 
                                                   best-micro
                                                   filtre-micro
                                                   inter->freq
                                                   ratio
                                                   ratio->cents
                                                   cents->ratio
                                                   diff->dist
                                                   string->list
                                                   midic->canal) nil)
                              ("OBJECT CONVERSIONS" nil nil (channel->micro
                                                             channel->voice
                                                             map-channel
                                                             seq-part
                                                             chord->notes
                                                             notes->chord
                                                             seq->notes
                                                             notes->seq
                                                             notes->multi-seq
                                                             explosion
                                                             implosion
                                                             sort-chords) nil)
                              ("FILES" nil nil (read-file
                                                write-file
                                                ll-write-file) nil)) nil nil nil)
        ("7-MIDI" nil nil (all-notes-off) nil)
        
        ("8-ANALYSIS/SYNTHESIS" (("Addi-MSP" nil nil (addi->coll
                                              addi->tfa
                                              addi->lfa
                                              lfa->coll) nil)
                         ("OM->spear" nil nil (matching
                                              spear-read
                                               spear-write
                                               spear-read-partials
                                               spear-make-partials
                                               spear-write-partials) nil)
                         ("SPDATA" nil nil (mask-read
                                            par-spdata
                                            mk-spdata
                                            mk-spdata-seq
                                            intpol-model
                                            filter-spdata
                                            filter-all) nil)) nil nil nil)

                         ))


