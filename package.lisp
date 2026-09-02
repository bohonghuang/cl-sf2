(defpackage cl-sf2
  (:use #:cl #:alexandria #:binstruct #:cliff)
  (:shadow #:read #:write)
  (:nicknames #:sf2)
  (:export #:read #:write))

(in-package #:sf2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enum / bitfield types  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinenum (generator (:type (unsigned-byte 16))) ()
  (start-addrs-offset 0)
  (end-addrs-offset 1)
  (startloop-addrs-offset 2)
  (endloop-addrs-offset 3)
  (start-addrs-coarse-offset 4)
  (end-addrs-coarse-offset 5)
  (startloop-addrs-coarse-offset 6)
  (endloop-addrs-coarse-offset 7)
  (unused1 8)
  (unused2 9)
  (unused3 10)
  (unused4 11)
  (unused5 12)
  (unused6 13)
  (unused7 14)
  (pan 15)
  (unused8 16)
  (unused9 17)
  (unused10 18)
  (unused11 19)
  (unused12 20)
  (delay-vol-env 21)
  (attack-vol-env 22)
  (hold-vol-env 23)
  (decay-vol-env 24)
  (sustain-vol-env 25)
  (release-vol-env 26)
  (keynum-to-vol-env-hold 27)
  (keynum-to-vol-env-decay 28)
  (unused13 29)
  (unused14 30)
  (unused15 31)
  (delay-mod-env 32)
  (attack-mod-env 33)
  (hold-mod-env 34)
  (decay-mod-env 35)
  (sustain-mod-env 36)
  (release-mod-env 37)
  (keynum-to-mod-env-hold 38)
  (keynum-to-mod-env-decay 39)
  (unused16 40)
  (unused17 41)
  (delay-mod-lfo 42)
  (freq-mod-lfo 43)
  (delay-vib-lfo 44)
  (freq-vib-lfo 45)
  (unused18 46)
  (unused19 47)
  (unused20 48)
  (unused21 49)
  (unused22 50)
  (unused23 51)
  (unused24 52)
  (unused25 53)
  (unused26 54)
  (unused27 55)
  (keynum 56)
  (velocity 57)
  (unused28 58)
  (keyrange 59)
  (velyrange 60)
  (attenuation 61)
  (end-oper 62))

(defbinenum (transform (:type (unsigned-byte 16))) ()
  (linear 0))

(defbinenum (sample-link (:type (unsigned-byte 16))) ()
  (no-sample-link 0)
  (mono-sample 1)
  (right-sample 2)
  (left-sample 4)
  (linked-sample 8)
  (rom-mono-sample #x8001)
  (rom-right-sample #x8002)
  (rom-left-sample #x8004)
  (rom-linked-sample #x8008))

(defbinstruct modulator ()
  (index 0 :type (unsigned-byte 7))
  (cc nil :type (boolean (unsigned-byte 1)))
  (direction 0 :type (unsigned-byte 1))
  (polarity 0 :type (unsigned-byte 1))
  (type 0 :type (unsigned-byte 6)))

(defbinstruct gen-amount ()
  (lo 0 :type (unsigned-byte 8))
  (hi 0 :type (unsigned-byte 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared helpers         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct ifil-rec ()
  (major 0 :type (unsigned-byte 16))
  (minor 0 :type (unsigned-byte 16)))

(defbinstruct (sized-simple-string (:type (simple-array character (*))) (:conc-name nil) (:constructor cliff::extract-values)) (bytes)
  (%start 0 :type position)
  (values #.(coerce "" 'simple-base-string) :type simple-string)
  (%end 0 :type position)
  (nil (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) ((- bytes (- %end %start))))))

(defbinstruct (s16vec (:type (simple-array (signed-byte 16) (*))) (:conc-name nil) (:constructor progn)) (length)
  (values (make-array 0 :element-type '(signed-byte 16))
          :type (simple-array (signed-byte 16) (length))))

(defbinio (s16vec length) (simple-array (unsigned-byte 8) (*)))

(defun u8vec-s16vec (u8vec)
  (read-s16vec u8vec (floor (length u8vec) 2)))

(defun s16vec-u8vec (s16vec)
  (let ((u8vec (make-array (* (length s16vec) 2)
                           :element-type '(unsigned-byte 8))))
    (write-s16vec u8vec s16vec (length s16vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFO chunks            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-riff-chunk (ifil (:magic "ifil"))
  (version (make-ifil-rec) :type ifil-rec))

(define-riff-chunk (iver (:magic "iver"))
  (version (make-ifil-rec) :type ifil-rec))

(define-riff-chunk (isng (:magic "isng"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (inam (:magic "INAM"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (irom (:magic "irom"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (icrd (:magic "ICRD"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (ieng (:magic "IENG"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (iprd (:magic "IPRD"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (icop (:magic "ICOP"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (icmt (:magic "ICMT"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

(define-riff-chunk (isft (:magic "ISFT"))
  (text "" :type (sized-simple-string (riff-chunk-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdta chunks            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-riff-chunk (smpl (:magic "smpl"))
  (data (make-array 0 :element-type '(signed-byte 16))
        :type (map (simple-array (unsigned-byte 8) ((riff-chunk-size)))
                   #'u8vec-s16vec #'s16vec-u8vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdta record structs    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct phdr-rec ()
  (name (make-array 20 :element-type 'base-char :initial-element #\Nul) :type (simple-base-string 20))
  (preset 0 :type (unsigned-byte 16))
  (bank 0 :type (unsigned-byte 16))
  (preset-bag-ndx 0 :type (unsigned-byte 16))
  (library 0 :type (unsigned-byte 32))
  (genre 0 :type (unsigned-byte 32))
  (morphology 0 :type (unsigned-byte 32)))

(defbinstruct pbag-rec ()
  (gen-ndx 0 :type (unsigned-byte 16))
  (mod-ndx 0 :type (unsigned-byte 16)))

(defbinstruct pmod-rec ()
  (src-oper (make-modulator) :type modulator)
  (dest-oper 'start-addrs-offset :type generator)
  (amount (make-gen-amount) :type gen-amount)
  (amt-src-oper (make-modulator) :type modulator)
  (trans-oper 'linear :type transform))

(defbinstruct pgen-rec ()
  (gen-oper 'start-addrs-offset :type generator)
  (amount (make-gen-amount) :type gen-amount))

(defbinstruct inst-rec ()
  (name (make-array 20 :element-type 'base-char :initial-element #\Nul) :type (simple-base-string 20))
  (bag-ndx 0 :type (unsigned-byte 16)))

(defbinstruct ibag-rec ()
  (gen-ndx 0 :type (unsigned-byte 16))
  (mod-ndx 0 :type (unsigned-byte 16)))

(defbinstruct imod-rec ()
  (src-oper (make-modulator) :type modulator)
  (dest-oper 'start-addrs-offset :type generator)
  (amount (make-gen-amount) :type gen-amount)
  (amt-src-oper (make-modulator) :type modulator)
  (trans-oper 'linear :type transform))

(defbinstruct igen-rec ()
  (gen-oper 'start-addrs-offset :type generator)
  (amount (make-gen-amount) :type gen-amount))

(defbinstruct shdr-rec ()
  (name (make-array 20 :element-type 'base-char :initial-element #\Nul) :type (simple-base-string 20))
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32))
  (start-loop 0 :type (unsigned-byte 32))
  (end-loop 0 :type (unsigned-byte 32))
  (sample-rate 0 :type (unsigned-byte 32))
  (original-pitch 0 :type (unsigned-byte 8))
  (pitch-correction 0 :type (signed-byte 8))
  (sample-link 0 :type (unsigned-byte 16))
  (sample-type 'no-sample-link :type sample-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdta chunks            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sizeof (name)
  (multiple-value-bind (min max)
      (parsonic::compute-bounds/compile
       (parsonic::expand/compile
        (ensure-list name)))
    (assert (= min max))
    min))

(define-riff-chunk (phdr (:magic "phdr"))
  (records (make-array 0 :element-type 'phdr-rec)
           :type (simple-array phdr-rec ((floor (riff-chunk-size) (sizeof phdr-rec))))))

(define-riff-chunk (pbag (:magic "pbag"))
  (records (make-array 0 :element-type 'pbag-rec)
           :type (simple-array pbag-rec ((floor (riff-chunk-size) (sizeof pbag-rec))))))

(define-riff-chunk (pmod (:magic "pmod"))
  (records (make-array 0 :element-type 'pmod-rec)
           :type (simple-array pmod-rec ((floor (riff-chunk-size) (sizeof pmod-rec))))))

(define-riff-chunk (pgen (:magic "pgen"))
  (records (make-array 0 :element-type 'pgen-rec)
           :type (simple-array pgen-rec ((floor (riff-chunk-size) (sizeof pgen-rec))))))

(define-riff-chunk (inst (:magic "inst"))
  (records (make-array 0 :element-type 'inst-rec)
           :type (simple-array inst-rec ((floor (riff-chunk-size) (sizeof inst-rec))))))

(define-riff-chunk (ibag (:magic "ibag"))
  (records (make-array 0 :element-type 'ibag-rec)
           :type (simple-array ibag-rec ((floor (riff-chunk-size) (sizeof ibag-rec))))))

(define-riff-chunk (imod (:magic "imod"))
  (records (make-array 0 :element-type 'imod-rec)
           :type (simple-array imod-rec ((floor (riff-chunk-size) (sizeof imod-rec))))))

(define-riff-chunk (igen (:magic "igen"))
  (records (make-array 0 :element-type 'igen-rec)
           :type (simple-array igen-rec ((floor (riff-chunk-size) (sizeof igen-rec))))))

(define-riff-chunk (shdr (:magic "shdr"))
  (records (make-array 0 :element-type 'shdr-rec)
           :type (simple-array shdr-rec ((floor (riff-chunk-size) (sizeof shdr-rec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST containers        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-riff-list (info-list (:magic "INFO"))
  ifil isng inam irom iver
  icrd ieng iprd icop icmt isft)

(define-riff-list (sdta-list (:magic "sdta"))
  smpl)

(define-riff-list (pdta-list (:magic "pdta"))
  phdr pbag pmod pgen inst
  ibag imod igen shdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level RIFF         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-riff-file (sfbk (:magic "sfbk"))
  info-list sdta-list pdta-list)

(defgeneric read (input)
  (:method ((stream stream))
    (read-sfbk-file stream))
  (:method ((vector vector))
    (read (make-instance 'fast-io:fast-input-stream :vector vector)))
  (:method ((pathname pathname))
    (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
      (read stream))))

(defgeneric write (object output)
  (:method ((object sfbk) (stream stream))
    (write-sfbk-file stream object))
  (:method ((object sfbk) (null null))
    (let ((stream (make-instance 'fast-io:fast-output-stream)))
      (write object stream)
      (fast-io:finish-output-stream stream)))
  (:method ((object sfbk) (pathname pathname))
    (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8))
      (write object stream))))
