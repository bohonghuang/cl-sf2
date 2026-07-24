;;;; u8 -> s16 little-endian conversion via a hand-written SBCL VOP.
;;;;
;;;; Standalone, x86-64 only.  Load with (load "u8->s16-vop.lisp") (which
;;;; compiles it).  The smpl chunk of a SoundFont 2 file stores samples as
;;;; (simple-array (signed-byte 16) (*)); on disk the raw byte stream is a
;;;; (simple-array (unsigned-byte 8) (*)) whose consecutive byte pairs are
;;;; signed 16-bit little-endian words.  This file decodes each pair in one
;;;; movsx :word instruction.

(in-package #:sb-vm)

(defknown sb-vm::%u8->s16/lea
    ((simple-array (unsigned-byte 8) (*))  ; src
     (unsigned-byte 64))                    ; element index of the lo byte (0..length/2-1)
    (signed-byte 16)
    (foldable flushable always-translatable)
  :overwrite-fndb-silently t)

;;; movsx :word on a 16-bit memory operand performs the little-endian load
;;; and sign-extension in one instruction: x86-64 is little-endian, so the
;;; word at byte offset (* INDEX 2) is exactly [lo hi].  We index by element
;;; (each output element = 2 source bytes) and use index-scale 2 so the EA
;;; scale compensates for the fixnum tag when INDEX is in any-reg (tagged)
;;; and uses a literal 2 when in unsigned-reg (untagged) — both yield a
;;; byte offset of INDEX*2.  This mirrors the simple-array-signed-byte-16
;;; reffer at src/compiler/x86-64/array.lisp:1029 but reads from a u8 array.
(define-vop (sb-vm::%u8->s16/lea)
  (:translate sb-vm::%u8->s16/lea)
  (:policy :fast-safe)
  (:args (src :scs (descriptor-reg))
         (index :scs (unsigned-reg signed-reg any-reg)))
  (:arg-types simple-array-unsigned-byte-8
              positive-fixnum)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx '(:word :qword) result
          (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
              src index (index-scale 2 index)))))

(defpackage #:cl-sf2/u8->s16-vop (:use #:cl) (:export #:u8->s16))
(in-package #:cl-sf2/u8->s16-vop)

(declaim (ftype (function ((simple-array (unsigned-byte 8) (*)))
                          (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16))
(defun u8->s16 (input)
  "Decode INPUT, a (simple-array (unsigned-byte 8) (*)) of even length, as a
\(simple-array (signed-byte 16) (*)) of half the length.  Each byte pair
\[lo hi] is read as a signed 16-bit little-endian word.  Odd length signals
an error."
  (declare (type (simple-array (unsigned-byte 8) (*)) input))
  (let ((n (length input)))
    (unless (evenp n)
      (error "u8->s16: odd length ~A" n))
    (let ((out (make-array (ash n -1) :element-type '(signed-byte 16))))
      (declare (type (simple-array (signed-byte 16) (*)) out))
      (loop for j fixnum below (ash n -1)
            do (setf (aref out j)
                     (sb-vm::%u8->s16/lea input (the (unsigned-byte 64) j))))
      out)))
