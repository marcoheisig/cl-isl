(ql:quickload :cl-isl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple intersection/union of sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; { [1] } and { [i] }

;; #<CL-ISL:UNION-SET { [1] }>
(let ((a (isl:union-set-from-str " { [1] } "))
      (b (isl:union-set-from-str " { [i] } ")))
  (isl:union-set-intersect a b))

;; #<CL-ISL:UNION-SET { [i] }>
(let ((a (isl:union-set-from-str " { [1] } "))
      (b (isl:union-set-from-str " { [i] } ")))
  (isl:union-set-union a b))


;; { [1] } and { [i]: i >= 10 }

;; #<CL-ISL:UNION-SET {  }>
(let ((a (isl:union-set-from-str " { [1] } "))
      (b (isl:union-set-from-str " { [i]: i >= 10 } ")))
  (isl:union-set-intersect a b))

;; <CL-ISL:UNION-SET { [i0] : i0 >= 10; [1] } >
(let ((a (isl:union-set-from-str " { [1] } "))
      (b (isl:union-set-from-str " { [i]: i >= 10 } ")))
  (isl:union-set-union a b))


;; { [i]: i >= -5 } and { [i]: i <= 5 }

;; #<CL-ISL:UNION-SET { [i] : -5 <= i <= 5 }>
(let ((a (isl:union-set-from-str " { [i]: i >= -5 } "))
      (b (isl:union-set-from-str " { [i]: i <= 5 } ")))
  (isl:union-set-intersect a b))

;; #<CL-ISL:UNION-SET { [i] : i >= -5 or i <= 5 }>
(let ((a (isl:union-set-from-str " { [i]: i >= -5 } "))
      (b (isl:union-set-from-str " { [i]: i <= 5 } ")))
  (isl:union-set-union a b))



;;;;;;;;;;;;;;;;;;;;;
;; Simple use of maps
;;;;;;;;;;;;;;;;;;;;;

;; #<CL-ISL:UNION-MAP { [i] -> [2 + i] }>
(let ((a (isl:union-map-from-str " { [i] -> [i + 1] } ")))
  (isl:union-map-apply-range a a))

;; #<CL-ISL:UNION-SET { B[6]; B[5] }>
(let ((a (isl:union-map-from-str "{ A [2 ,8 ,1] -> B [5]; A [2 ,8 ,1] -> B [6]; B [5] -> B [5] } "))
      (b (isl:union-set-from-str " { A [2 ,8 ,1]; B [5] } ")))
  (isl:union-set-apply b a))

;; #<CL-ISL:UNION-SET { [i0] : i0 <= 10 }>
(let ((a (isl:union-map-from-str " { [i] -> [i+1] } "))
      (b (isl:union-set-from-str " { [i]: i <= 9 } ")))
  (isl:union-set-apply b a))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example of utilisation
;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-isl)

;; The domain of iteration of iteration the outer loop is { [i, j] : 1 <= i < 10 and 1 <= j < 10 }
(setf domain-outer (union-set-from-str " { [i, j] : 1 <= i < 10 and 1 <= j < 10 } "))
;; #<UNION-SET { [i, j] : 0 < i <= 9 and 0 < j <= 9 }>

;; The domain of iteration of iteration the inner loop is [i] -> { [i, j] : 1 <= j < 10 }
(setf domain-inner (union-set-from-str " [i] -> { [i, j] : 1 <= j < 10 } "))
;; #<UNION-SET [i] -> { [i, j] : 0 < j <= 9 }>

;; What is read in the instruction in the inner loop is { [i, j] -> array[1] }
(setf vread (union-map-from-str " { [i, j] -> array[1] } "))
;; #<UNION-MAP { [i, j] -> array[1] }>

;; What is written in the instruction in the inner loop is { [i, j] -> array[i+j] }
(setf vwrite (union-map-from-str " { [i, j] -> array[i+j] } "))
;; #<UNION-MAP { [i, j] -> array[i + j] }>

;; The total of the thing written in the innerloop is [i] -> { array[i0] : i < i0 <= 9 + i }
(setf all-written-inner (union-set-apply domain-inner vwrite))
;; #<UNION-SET [i] -> { array[i0] : i < i0 <= 9 + i }>

;; Let's compute the intersection for the innerloop, it's [i] -> { array[1] : -8 <= i <= 0 }
(setf intersection-inner (union-set-intersect (union-set-apply domain-inner vread) all-written-inner))
;; #<UNION-SET [i] -> { array[1] : -8 <= i <= 0 }>

;; Because the result depends on i, let's add the information that i is between 1 and 10. The result is the empty set! So we can move this statement outside the innerloop.
(union-set-intersect-params intersection-inner (set-from-str "[i] -> {: 1 <= i < 10}"))
;; #<UNION-SET [i] -> {  }>

;; Just to double check, if the outerloop is from 0 to 10, the result would be [i] -> { array[1] : i = 0 }
(union-set-intersect-params intersection-inner (set-from-str "[i] -> {: 0 <= i < 10}"))
;; #<UNION-SET [i] -> { array[1] : i = 0 }>

;; Also, the total of the thing written in the outerloop is { array[i0] : 0 < i0 <= 18 and (i0 >= 2 or i0 <= 9) }
(setf all-written-outer (union-set-union (union-set-apply domain-outer vwrite) (union-set-from-str " {array[i]: 1 <= i < 10}")))
;; #<UNION-SET { array[i0] : 0 < i0 <= 18 and (i0 >= 2 or i0 <= 9) }>

;; So the intersection with { array[1] } is non empty! So we can't move outside the outerloop
(union-set-intersect (union-set-from-str " { array[1] }") all-written-outer)
;; #<UNION-SET { array[1] }>
