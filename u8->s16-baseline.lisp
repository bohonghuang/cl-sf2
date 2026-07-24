;;;; u8 -> s16 little-endian conversion — optimized portable Common Lisp baseline.
;;;;
;;;; Uses dpb to merge byte pairs (the same pattern SBCL's own UCS-2LE
;;;; decoder uses in src/code/external-formats/enc-ucs.lisp) and a branchless
;;;; sign-extension.  On SBCL, sb-c:mask-signed-field compiles to a single
;;;; MOVSX instruction (hardware sign-extension); on other implementations a
;;;; portable dpb-into-(-1) fallback is used.  Declares simple-array element
;;;; types and (speed 3) (safety 0) to match the VOP and SIMD files.  No VOP,
;;;; no SIMD — this is the optimized pure-CL reference against which the VOP
;;;; and SIMD implementations are benchmarked.

(defpackage #:cl-sf2/u8->s16-baseline
  (:use #:cl)
  (:export #:u8->s16))
(in-package #:cl-sf2/u8->s16-baseline)

(declaim (ftype (function ((simple-array (unsigned-byte 8) (*)))
                          (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16))
(defun u8->s16 (input)
  "Decode INPUT, a (simple-array (unsigned-byte 8)) of even length, as a
\(simple-array (signed-byte 16)) of half the length.  Each byte pair [lo hi]
is read as a signed 16-bit little-endian word.  Odd length signals an error."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8)) input))
  (let ((n (length input)))
    (unless (evenp n)
      (error "u8->s16: odd length ~A" n))
    (let ((out (make-array (ash n -1) :element-type '(signed-byte 16))))
      (declare (type (simple-array (signed-byte 16) (*)) out))
      (loop for j fixnum below (ash n -1)
            for k fixnum from 0 by 2
            do (setf (aref out j)
                     (let ((u16 (dpb (aref input (1+ k)) (byte 8 8) (aref input k))))
                       (declare (type (unsigned-byte 16) u16))
                       #+sbcl
                       (sb-c::mask-signed-field 16 u16)
                       #-sbcl
                       (if (logbitp 15 u16)
                           (dpb u16 (byte 16 0) -1)
                           u16))))
      out)))
