(defpackage cl-sf2
  (:use #:cl #:alexandria #:binstruct)
  (:nicknames #:sf2)
  (:export #:read-sf2 #:write-sf2))

(in-package #:sf2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base RIFF chunk primitive ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct sf2-chunk (id)
  (nil id :type (satisfies (simple-base-string 4)))
  (size 0 :type (unsigned-byte 32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enum / bitfield types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SF2 generator enumerator (spec §8.1.2), total over 0..62.
;; Reserved indices get placeholder symbols so the enum is total.
(defbinenum (sf2-generator (:type (unsigned-byte 16))) ()
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

;; SF2 transform (spec §8.3): only linear = 0 is standard.
(defbinenum (sf2-transform (:type (unsigned-byte 16))) ()
  (linear 0))

;; SF2 sample link type (spec §8.4.2).
(defbinenum (sf2-sample-link (:type (unsigned-byte 16))) ()
  (no-sample-link 0)
  (mono-sample 1)
  (right-sample 2)
  (left-sample 4)
  (linked-sample 8)
  (rom-mono-sample #x8001)
  (rom-right-sample #x8002)
  (rom-left-sample #x8004)
  (rom-linked-sample #x8008))

;; SF2 modulator bitfield (spec §8.2): 16 bits packed LSB-first.
;; bits 0-6 index, 7 CC, 8 direction, 9 polarity, 10-15 type.
(defbinstruct sf2-modulator ()
  (index 0 :type (unsigned-byte 7))
  (cc nil :type (boolean (unsigned-byte 1)))
  (direction 0 :type (unsigned-byte 1))
  (polarity 0 :type (unsigned-byte 1))
  (type 0 :type (unsigned-byte 6)))

;; SF2 generator amount: raw two-byte form (rangesType / SHORT / WORD).
;; Keeps round-trip exact regardless of generator kind.
(defbinstruct sf2-gen-amount ()
  (lo 0 :type (unsigned-byte 8))
  (hi 0 :type (unsigned-byte 8)))

(declaim (inline sf2-gen-amount-shamount))
(defun sf2-gen-amount-shamount (amount)
  "Decode the two bytes of AMOUNT as a signed 16-bit little-endian value."
  (declare (type sf2-gen-amount amount))
  (logior (sf2-gen-amount-lo amount)
          (ash (let ((hi (sf2-gen-amount-hi amount)))
                 (if (logbitp 7 hi) (- hi 256) hi)) 8)))

(declaim (inline sf2-gen-amount-wamount))
(defun sf2-gen-amount-wamount (amount)
  "Decode the two bytes of AMOUNT as an unsigned 16-bit little-endian value."
  (declare (type sf2-gen-amount amount))
  (logior (sf2-gen-amount-lo amount)
          (ash (sf2-gen-amount-hi amount) 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFO-list sub-chunks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct sf2-ifil-rec ()
  (major 0 :type (unsigned-byte 16))
  (minor 0 :type (unsigned-byte 16)))

(defbinstruct (sf2-ifil (:include (sf2-chunk #.(coerce "ifil" 'simple-base-string)))) ()
  (version (make-sf2-ifil-rec) :type sf2-ifil-rec))

(defbinstruct (sf2-iver (:include (sf2-chunk #.(coerce "iver" 'simple-base-string)))) ()
  (version (make-sf2-ifil-rec) :type sf2-ifil-rec))

(defbinstruct (sf2-isng (:include (sf2-chunk #.(coerce "isng" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-inam (:include (sf2-chunk #.(coerce "INAM" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-irom (:include (sf2-chunk #.(coerce "irom" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-icrd (:include (sf2-chunk #.(coerce "ICRD" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-ieng (:include (sf2-chunk #.(coerce "IENG" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-iprd (:include (sf2-chunk #.(coerce "IPRD" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-icop (:include (sf2-chunk #.(coerce "ICOP" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-icmt (:include (sf2-chunk #.(coerce "ICMT" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

(defbinstruct (sf2-isft (:include (sf2-chunk #.(coerce "ISFT" 'simple-base-string)))) ()
  (text (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdta-list sub-chunk ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct (sf2-smpl (:include (sf2-chunk #.(coerce "smpl" 'simple-base-string)))) ()
  (data (make-array 0 :element-type '(signed-byte 16))
        :type (simple-array (signed-byte 16) ((floor size 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdta-list record structs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct sf2-phdr-rec ()
  (name (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0) :type (simple-array (unsigned-byte 8) (20)))
  (preset 0 :type (unsigned-byte 16))
  (bank 0 :type (unsigned-byte 16))
  (preset-bag-ndx 0 :type (unsigned-byte 16))
  (library 0 :type (unsigned-byte 32))
  (genre 0 :type (unsigned-byte 32))
  (morphology 0 :type (unsigned-byte 32)))

(defbinstruct sf2-pbag-rec ()
  (gen-ndx 0 :type (unsigned-byte 16))
  (mod-ndx 0 :type (unsigned-byte 16)))

(defbinstruct sf2-pmod-rec ()
  (src-oper (make-sf2-modulator) :type sf2-modulator)
  (dest-oper 'start-addrs-offset :type sf2-generator)
  (amount (make-sf2-gen-amount) :type sf2-gen-amount)
  (amt-src-oper (make-sf2-modulator) :type sf2-modulator)
  (trans-oper 'linear :type sf2-transform))

(defbinstruct sf2-pgen-rec ()
  (gen-oper 'start-addrs-offset :type sf2-generator)
  (amount (make-sf2-gen-amount) :type sf2-gen-amount))

(defbinstruct sf2-inst-rec ()
  (name (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0) :type (simple-array (unsigned-byte 8) (20)))
  (bag-ndx 0 :type (unsigned-byte 16)))

(defbinstruct sf2-ibag-rec ()
  (gen-ndx 0 :type (unsigned-byte 16))
  (mod-ndx 0 :type (unsigned-byte 16)))

(defbinstruct sf2-imod-rec ()
  (src-oper (make-sf2-modulator) :type sf2-modulator)
  (dest-oper 'start-addrs-offset :type sf2-generator)
  (amount (make-sf2-gen-amount) :type sf2-gen-amount)
  (amt-src-oper (make-sf2-modulator) :type sf2-modulator)
  (trans-oper 'linear :type sf2-transform))

(defbinstruct sf2-igen-rec ()
  (gen-oper 'start-addrs-offset :type sf2-generator)
  (amount (make-sf2-gen-amount) :type sf2-gen-amount))

(defbinstruct sf2-shdr-rec ()
  (name (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0) :type (simple-array (unsigned-byte 8) (20)))
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32))
  (start-loop 0 :type (unsigned-byte 32))
  (end-loop 0 :type (unsigned-byte 32))
  (sample-rate 0 :type (unsigned-byte 32))
  (original-pitch 0 :type (unsigned-byte 8))
  (pitch-correction 0 :type (signed-byte 8))
  (sample-link 0 :type (unsigned-byte 16))
  (sample-type 'no-sample-link :type sf2-sample-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdta-list chunk structs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinstruct (sf2-phdr (:include (sf2-chunk #.(coerce "phdr" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-phdr-rec) :type (simple-array sf2-phdr-rec ((floor size 38)))))

(defbinstruct (sf2-pbag (:include (sf2-chunk #.(coerce "pbag" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-pbag-rec) :type (simple-array sf2-pbag-rec ((floor size 4)))))

(defbinstruct (sf2-pmod (:include (sf2-chunk #.(coerce "pmod" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-pmod-rec) :type (simple-array sf2-pmod-rec ((floor size 10)))))

(defbinstruct (sf2-pgen (:include (sf2-chunk #.(coerce "pgen" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-pgen-rec) :type (simple-array sf2-pgen-rec ((floor size 4)))))

(defbinstruct (sf2-inst (:include (sf2-chunk #.(coerce "inst" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-inst-rec) :type (simple-array sf2-inst-rec ((floor size 22)))))

(defbinstruct (sf2-ibag (:include (sf2-chunk #.(coerce "ibag" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-ibag-rec) :type (simple-array sf2-ibag-rec ((floor size 4)))))

(defbinstruct (sf2-imod (:include (sf2-chunk #.(coerce "imod" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-imod-rec) :type (simple-array sf2-imod-rec ((floor size 10)))))

(defbinstruct (sf2-igen (:include (sf2-chunk #.(coerce "igen" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-igen-rec) :type (simple-array sf2-igen-rec ((floor size 4)))))

(defbinstruct (sf2-shdr (:include (sf2-chunk #.(coerce "shdr" 'simple-base-string)))) ()
  (records (make-array 0 :element-type 'sf2-shdr-rec) :type (simple-array sf2-shdr-rec ((floor size 46)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST containers and top-level RIFF ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INFO-list: sentinel-terminated sub-chunk array with (or ...) dispatch on 4-byte id.
(defbinstruct sf2-info-subchunk (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (chunk nil :type (or sf2-ifil sf2-isng sf2-inam sf2-irom sf2-iver
                      sf2-icrd sf2-ieng sf2-iprd sf2-icop sf2-icmt sf2-isft)))

(defbinstruct (sf2-info-list (:include (sf2-chunk #.(coerce "LIST" 'simple-base-string)))) ()
  (form #.(coerce "INFO" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (end 0 :type (map position (curry #'+ (- size 4))))
  (chunks (make-array 0 :element-type 'sf2-info-subchunk)
          :type (simple-array (sf2-info-subchunk end) (*))))

;; sdta-list: single smpl member.
(defbinstruct sf2-sdta-subchunk (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (chunk nil :type (or sf2-smpl)))

(defbinstruct (sf2-sdta-list (:include (sf2-chunk #.(coerce "LIST" 'simple-base-string)))) ()
  (form #.(coerce "sdta" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (end 0 :type (map position (curry #'+ (- size 4))))
  (chunks (make-array 0 :element-type 'sf2-sdta-subchunk)
          :type (simple-array (sf2-sdta-subchunk end) (*))))

;; pdta-list: nine hydra chunk members.
(defbinstruct sf2-pdta-subchunk (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (chunk nil :type (or sf2-phdr sf2-pbag sf2-pmod sf2-pgen sf2-inst
                      sf2-ibag sf2-imod sf2-igen sf2-shdr)))

(defbinstruct (sf2-pdta-list (:include (sf2-chunk #.(coerce "LIST" 'simple-base-string)))) ()
  (form #.(coerce "pdta" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (end 0 :type (map position (curry #'+ (- size 4))))
  (chunks (make-array 0 :element-type 'sf2-pdta-subchunk)
          :type (simple-array (sf2-pdta-subchunk end) (*))))

;; Top-level RIFF.
(defbinstruct (sf2-riff (:include (sf2-chunk #.(coerce "RIFF" 'simple-base-string)))) ()
  (form #.(coerce "sfbk" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (info (make-sf2-info-list) :type sf2-info-list)
  (sdta (make-sf2-sdta-list) :type sf2-sdta-list)
  (pdta (make-sf2-pdta-list) :type sf2-pdta-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defbinio and entry points ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbinio (sf2-riff) stream)

(defun read-sf2 (stream)
  (read-sf2-riff stream))

(defun write-sf2 (stream sf2)
  (write-sf2-riff stream sf2))
