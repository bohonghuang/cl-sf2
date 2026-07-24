;;;; Benchmark: portable baseline vs VOP vs SIMD.
(load "u8->s16-baseline.lisp")
(load "u8->s16-vop.lisp")
(load "u8->s16-simd.lisp")

(defun time-it (fn input iters)
  (funcall fn input)
  (gc :full t)
  (let ((start (get-internal-real-time)))
    (dotimes (i iters)
      (funcall fn input))
    (let ((elapsed (- (get-internal-real-time) start)))
      (/ elapsed internal-time-units-per-second))))

(defun make-input (n)
  (make-array n :element-type '(unsigned-byte 8)
              :initial-contents (loop repeat n collect (random 256))))

(sb-ext:seed-random-state (make-random-state))

(format t "~&=== u8->s16 benchmark: baseline vs VOP vs SIMD ===~%")
(format t "~&Active SIMD path: ~A~%"
        (sb-simd:instruction-set-case
          (:avx2 :avx2) (:sse2 :sse2) (:x86-64 :scalar)))

(dolist (n '(1024 4096 16384 65536 262144 1048576))
  (let* ((input (make-input n))
         (out-len (ash n -1))
         (iters (max 100 (min 100000 (floor 200000000 n))))
         (t-base (time-it #'cl-sf2/u8->s16-baseline:u8->s16 input iters))
         (t-vop  (time-it #'cl-sf2/u8->s16-vop:u8->s16 input iters))
         (t-simd (time-it #'cl-sf2/u8->s16-simd:u8->s16 input iters)))
    (format t "~&~8D bytes -> ~7D s16 | iters=~6D |~%" n out-len iters)
    (format t "~&  baseline: ~10,6F s  (~8,2f MB/s)~%" t-base (/ (* n iters) t-base 1d6))
    (format t "~&  vop:      ~10,6F s  (~8,2f MB/s)~%" t-vop  (/ (* n iters) t-vop  1d6))
    (format t "~&  simd:     ~10,6F s  (~8,2f MB/s)~%" t-simd (/ (* n iters) t-simd 1d6))
    (format t "~&  speedup:  vop=~5,2fx  simd=~5,2fx (vs baseline)~%"
            (/ t-base t-vop) (/ t-base t-simd))))
