(defpackage #:cl-sf2.test
  (:use #:cl #:parachute #:binstruct)
  (:import-from #:alexandria #:read-file-into-byte-vector)
  (:import-from #:sf2
   #:read-sf2 #:write-sf2
   #:sf2-riff-form #:sf2-riff-info #:sf2-riff-sdta #:sf2-riff-pdta
   #:sf2-info-list-form #:sf2-info-list-chunks
   #:sf2-sdta-list-form #:sf2-sdta-list-chunks
   #:sf2-pdta-list-form #:sf2-pdta-list-chunks
   #:sf2-info-subchunk-chunk
   #:sf2-sdta-subchunk-chunk
   #:sf2-pdta-subchunk-chunk
   #:sf2-ifil-version
   #:sf2-ifil-rec-major #:sf2-ifil-rec-minor
   #:sf2-phdr-records #:sf2-shdr-records
   #:sf2-phdr-rec-name
   #:sf2-shdr-rec-name)
  (:import-from #:flexi-streams
   #:make-in-memory-input-stream
   #:make-in-memory-output-stream
   #:get-output-stream-sequence)
  (:nicknames #:sf2.test))

(in-package #:sf2.test)

(define-test suite)

(defun sf2-input-stream ()
  "Return an in-memory binary input stream over the real SF2 file."
  (make-in-memory-input-stream
   (read-file-into-byte-vector "~/Downloads/pkmnfrlg.sf2")))

(defun decode-name (bytes)
  "Decode a 20-byte name field (null-terminated ASCII) to a string."
  (let ((end (or (position 0 bytes) (length bytes))))
    (map 'string #'code-char (subseq bytes 0 end))))

(define-test sf2-read :parent suite
  (let ((sf2 (read-sf2 (sf2-input-stream))))
    ;; Top-level RIFF
    (is string= "sfbk" (sf2-riff-form sf2))
    ;; INFO list
    (let ((info (sf2-riff-info sf2)))
      (is string= "INFO" (sf2-info-list-form info))
      (true (plusp (length (sf2-info-list-chunks info)))
            "INFO list should have sub-chunks")
      ;; ifil is always the first INFO sub-chunk
      (let* ((sub (aref (sf2-info-list-chunks info) 0))
             (ifil (sf2-info-subchunk-chunk sub)))
        (true (string= "SF2-IFIL" (string (type-of ifil))) "first INFO chunk should be ifil")
        (let ((ver (sf2-ifil-version ifil)))
          (is = 2 (sf2-ifil-rec-major ver))
          (is = 1 (sf2-ifil-rec-minor ver)))))
    ;; sdta list
    (let ((sdta (sf2-riff-sdta sf2)))
      (is string= "sdta" (sf2-sdta-list-form sdta))
      (is = 1 (length (sf2-sdta-list-chunks sdta))
          "sdta list should have exactly one smpl chunk"))
    ;; pdta list
    (let ((pdta (sf2-riff-pdta sf2)))
      (is string= "pdta" (sf2-pdta-list-form pdta))
      (is = 9 (length (sf2-pdta-list-chunks pdta))
          "pdta list should have 9 hydra chunks")
      ;; Verify chunk types in order
      (let ((type-names (loop for sub across (sf2-pdta-list-chunks pdta)
                              collect (string (type-of (sf2-pdta-subchunk-chunk sub))))))
        (is equal
            '("SF2-PHDR" "SF2-PBAG" "SF2-PMOD" "SF2-PGEN" "SF2-INST"
              "SF2-IBAG" "SF2-IMOD" "SF2-IGEN" "SF2-SHDR")
            type-names))
      ;; phdr array length >= 2
      (let ((phdr (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks pdta) 0))))
        (true (>= (length (sf2-phdr-records phdr)) 2)
              "phdr should have at least 2 records")
        ;; First preset name is non-empty
        (let ((name (decode-name (sf2-phdr-rec-name (aref (sf2-phdr-records phdr) 0)))))
          (true (plusp (length name))
                "first preset name should be non-empty")))
      ;; shdr array length >= 2
      (let ((shdr (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks pdta) 8))))
        (true (>= (length (sf2-shdr-records shdr)) 2)
              "shdr should have at least 2 records")))))

(define-test sf2-roundtrip :parent suite
  (let* ((sf2 (read-sf2 (sf2-input-stream)))
         (out-stream (make-in-memory-output-stream))
         (result-stream (write-sf2 out-stream sf2))
         (out-bytes (get-output-stream-sequence result-stream)))
    (let ((sf2-2 (read-sf2 (make-in-memory-input-stream out-bytes))))
      ;; Top-level form strings match
      (is string= (sf2-riff-form sf2) (sf2-riff-form sf2-2))
      ;; INFO form matches
      (is string= (sf2-info-list-form (sf2-riff-info sf2))
          (sf2-info-list-form (sf2-riff-info sf2-2)))
      ;; sdta form matches
      (is string= (sf2-sdta-list-form (sf2-riff-sdta sf2))
          (sf2-sdta-list-form (sf2-riff-sdta sf2-2)))
      ;; pdta form matches
      (is string= (sf2-pdta-list-form (sf2-riff-pdta sf2))
          (sf2-pdta-list-form (sf2-riff-pdta sf2-2)))
      ;; INFO chunk count matches
      (is = (length (sf2-info-list-chunks (sf2-riff-info sf2)))
          (length (sf2-info-list-chunks (sf2-riff-info sf2-2)))
          "INFO chunk count should match after roundtrip")
      ;; pdta chunk count matches
      (is = (length (sf2-pdta-list-chunks (sf2-riff-pdta sf2)))
          (length (sf2-pdta-list-chunks (sf2-riff-pdta sf2-2)))
          "pdta chunk count should match after roundtrip")
      ;; phdr record count matches
      (let ((phdr-1 (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks (sf2-riff-pdta sf2)) 0)))
            (phdr-2 (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks (sf2-riff-pdta sf2-2)) 0))))
        (is = (length (sf2-phdr-records phdr-1)) (length (sf2-phdr-records phdr-2))
            "phdr record count should match after roundtrip"))
      ;; shdr record count matches
      (let ((shdr-1 (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks (sf2-riff-pdta sf2)) 8)))
            (shdr-2 (sf2-pdta-subchunk-chunk (aref (sf2-pdta-list-chunks (sf2-riff-pdta sf2-2)) 8))))
        (is = (length (sf2-shdr-records shdr-1)) (length (sf2-shdr-records shdr-2))
            "shdr record count should match after roundtrip")))))
