;;;; u8 -> s16 little-endian conversion via sb-simd AVX2/SSE2.
;;;;
;;;; Standalone, x86-64 only.  Load with (load "u8->s16-simd.lisp") after
;;;; (require 'sb-simd).  The conversion is a memory reinterpretation: two
;;;; consecutive u8 bytes [lo hi] ARE a little-endian signed 16-bit word in
;;;; memory, so the SIMD op is load-raw-u8-pack, reinterpret the same bits as
;;;; an s16 pack (zero cost — same XMM/YMM register), store into the s16
;;;; array.  No pmovsxbw sign-extension (that would treat each u8 as a
;;;; separate sample, which is wrong).

(require 'sb-simd)

(defpackage #:cl-sf2/u8->s16-simd
  (:use #:cl #:sb-simd)
  (:export #:u8->s16))
(in-package #:cl-sf2/u8->s16-simd)
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))
                          (simple-array (signed-byte 16) (*))
                          (integer 0))
                         (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16-avx2 u8->s16-sse2 u8->s16-scalar))

(declaim (ftype (function ((simple-array (unsigned-byte 8) (*)))
                          (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16))
(defun u8->s16 (input)
  "Decode INPUT, a (simple-array (unsigned-byte 8) (*)) of even length, as a
\(simple-array (signed-byte 16) (*)) of half the length.  Each byte pair
\[lo hi] is read as a signed 16-bit little-endian word.  Dispatches to AVX2,
SSE2, or a scalar loop based on the CPU's available instruction sets.  Odd
length signals an error."
  (declare (type (simple-array (unsigned-byte 8) (*)) input))
  (let ((n (length input)))
    (unless (evenp n)
      (error "u8->s16: odd length ~A" n))
    (let ((out (make-array (ash n -1) :element-type '(signed-byte 16))))
      (declare (type (simple-array (signed-byte 16) (*)) out))
      (instruction-set-case
        (:avx2  (u8->s16-avx2 input out n))
        (:sse2  (u8->s16-sse2 input out n))
        (:x86-64 (u8->s16-scalar input out n)))
      out)))

;;; AVX2: 32 u8 -> 16 s16 per iteration.  u8.32-row-major-aref takes a byte
;;; index (32 bytes per load); s16.16-row-major-aref takes an element index
;;; (16 elements per store).  s16.16! reinterprets the 256-bit u8.32 pack as
;;; s16.16 with a zero-cost vmovdqu move.
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))
                          (simple-array (signed-byte 16) (*))
                          (integer 0))
                         (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16-avx2))
(defun u8->s16-avx2 (input out n)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (signed-byte 16) (*)) out)
           (type (integer 0) n))
  (loop for i fixnum below (floor n 32)
        do (setf (sb-simd-avx:s16.16-row-major-aref out (* i 16))
                 (sb-simd-avx:s16.16!
                  (sb-simd-avx:u8.32-row-major-aref input (* i 32)))))
  ;; Scalar tail for the remaining 0..31 bytes (0..15 output elements).
  (let ((tail-start (* (floor n 32) 32)))
    (loop for k fixnum from tail-start below n by 2
          for j fixnum from (ash tail-start -1)
          do (let ((lo (aref input k)) (hi (aref input (1+ k))))
               (setf (aref out j)
                     (if (logbitp 7 hi)
                         (- (logior lo (ash hi 8)) 65536)
                         (logior lo (ash hi 8)))))))
  out)

;;; SSE2: 16 u8 -> 8 s16 per iteration.  Same reinterpret-cast logic at
;;; 128 bits; s16.8! reinterprets u8.16 as s16.8 via movdqu.
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))
                          (simple-array (signed-byte 16) (*))
                          (integer 0))
                         (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16-sse2))
(defun u8->s16-sse2 (input out n)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (signed-byte 16) (*)) out)
           (type (integer 0) n))
  (loop for i fixnum below (floor n 16)
        do (setf (sb-simd-sse2:s16.8-row-major-aref out (* i 8))
                 (sb-simd-sse2:s16.8!
                  (sb-simd-sse2:u8.16-row-major-aref input (* i 16)))))
  ;; Scalar tail for the remaining 0..15 bytes (0..7 output elements).
  (let ((tail-start (* (floor n 16) 16)))
    (loop for k fixnum from tail-start below n by 2
          for j fixnum from (ash tail-start -1)
          do (let ((lo (aref input k)) (hi (aref input (1+ k))))
               (setf (aref out j)
                     (if (logbitp 7 hi)
                         (- (logior lo (ash hi 8)) 65536)
                         (logior lo (ash hi 8)))))))
  out)

;;; Scalar fallback (also the correctness oracle).  Used when neither AVX2
;;; nor SSE2 is available — the :x86-64 base instruction set is always
;;; present on x86-64.
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))
                          (simple-array (signed-byte 16) (*))
                          (integer 0))
                         (values (simple-array (signed-byte 16) (*)) &optional))
                u8->s16-scalar))
(defun u8->s16-scalar (input out n)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type (simple-array (signed-byte 16) (*)) out)
           (type (integer 0) n))
  (loop for j fixnum below (ash n -1)
        for k fixnum from 0 by 2
        do (let ((lo (aref input k)) (hi (aref input (1+ k))))
             (setf (aref out j)
                   (if (logbitp 7 hi)
                       (- (logior lo (ash hi 8)) 65536)
                       (logior lo (ash hi 8))))))
  out)
