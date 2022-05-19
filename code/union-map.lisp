(in-package :cl-isl)

(define-isl-object union-map
  :free %isl-union-map-free
  :copy %isl-union-map-copy
  :list-type union-map-list
  :from-str t)

(defmethod print-object ((value union-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-map-to-str (union-map-handle value)) stream)))

;; Creation

(define-isl-function union-map-empty %isl-union-map-empty
  (:give union-map)
  (:take space))

(define-isl-function union-map-universe %isl-union-map-universe
  (:give union-map)
  (:take space))

;; Conversion

(define-isl-function basic-map-union-map %isl-union-map-from-basic-map
  (:give union-map)
  (:take basic-map))

(define-isl-function map-union-map %isl-union-map-from-map
  (:give union-map)
  (:take map))

;; Operations

;; map -> map

(define-isl-function union-map-reverse %isl-union-map-reverse
  (:give union-map)
  (:take union-map))

;; (map, map) -> map

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give union-map)
                (:take union-map)
                (:take union-map))))
  (def union-map-intersect %isl-union-map-intersect)
  (def union-map-union %isl-union-map-union)
  (def union-map-subtract %isl-union-map-subtract)
  (def union-map-apply-range %isl-union-map-apply-range)
  (def union-map-apply-domain %isl-union-map-apply-domain)
  (def union-map-product %isl-union-map-product)
  (def union-map-lex-lt-union-map %isl-union-map-lex-lt-union-map)
  (def union-map-lex-le-union-map %isl-union-map-lex-le-union-map)
  (def union-map-lex-gt-union-map %isl-union-map-lex-gt-union-map)
  (def union-map-lex-ge-union-map %isl-union-map-lex-ge-union-map))

;; (map, map) -> bool

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give boolean)
                (:take union-map)
                (:take union-map))))
  (def union-map-is-equal %isl-union-map-is-equal)
  (def union-map-<= %isl-union-map-is-subset)
  (def union-map-< %isl-union-map-is-strict-subset)
  ;;(def union-map->= (lambda (a b) (%isl-union-map-is-submap a b)))
  ;;(def union-map-> nil)
  )

;; Domain and range

(define-isl-function union-map-domain %isl-union-map-domain
  (:give union-set)
  (:take union-map domain))

(define-isl-function union-map-range %isl-union-map-range
  (:give union-set)
  (:take union-map range))

(define-isl-function union-map-from-domain-and-range %isl-union-map-from-domain-and-range
  (:give union-map)
  (:take union-set domain)
  (:take union-set range))

(define-isl-function union-set-identity %isl-union-set-identity
  (:give union-map)
  (:take union-set))

(DEFINE-ISL-FUNCTION UNION-MAP-INTERSECT-DOMAIN %ISL-UNION-MAP-INTERSECT-DOMAIN
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-SET DOMAIN))
(DEFINE-ISL-FUNCTION UNION-MAP-INTERSECT-RANGE %ISL-UNION-MAP-INTERSECT-RANGE
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-SET range))
(DEFINE-ISL-FUNCTION UNION-MAP-SUBTRACT-DOMAIN %ISL-UNION-MAP-SUBTRACT-DOMAIN
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-SET DOMAIN))
(DEFINE-ISL-FUNCTION UNION-MAP-SUBTRACT-RANGE %ISL-UNION-MAP-SUBTRACT-RANGE
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-SET range))

(define-isl-function union-set-apply %isl-union-set-apply
  (:give union-set)
  (:take union-map)
  (:take union-set))

(DEFINE-ISL-FUNCTION UNION-MAP-APPLY-RANGE %ISL-UNION-MAP-APPLY-RANGE
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-MAP range))

(DEFINE-ISL-FUNCTION UNION-MAP-APPLY-DOMAIN %ISL-UNION-MAP-APPLY-DOMAIN
  (:GIVE UNION-MAP)
  (:TAKE UNION-MAP)
  (:TAKE UNION-MAP domain))
