#lang plai

(require "l4-definition.rkt" "../l3c/l3-definition.rkt" "../l3c/l3-parser.rkt")
(require "l4-parser.rkt")

(define/contract (l4-compile-prog p)
  (-> l4prog? l3prog?)
  (l3prog (l4-normalize (l4prog-l4entry p))
          (map l4-compile-func (l4prog-l4funcl p))))

(define/contract (l4-compile-func f)
  (-> l4func? l3func?)
  (l3func (l4func-l4fname f)
          (l4func-l4fvarl f)
          (length (l4func-l4fvarl f))
          (l4-normalize (l4func-l4fbody f))))

(define/contract (l4-normalize e)
  (-> L4Expression? L3Expression?)
  (l4-find e (l4noctxt)))

(define/contract (l4-find e k)
  (-> L4Expression? L4Context? L3Expression?)
  (type-case L4Expression e
    [l4let    (var valu body)
              (l4-find valu (l4letctxt var body k))]
    [l4if     (cond then else)
              (l4-find cond (l4ifctxt then else k))]
    [l4biop   (oper lhs rhs)
              (l4-find lhs (l4bilctxt oper rhs k))]
    [l4pred   (oper valu)
              (l4-find valu (l4prdctxt oper k))]
    [l4funcal (func argl)
              (l4-find func (l4functxt argl k))]
    [l4newarr (size valu)
              (l4-find size (l4nasctxt valu k))]
    [l4newtup (vall)
              (l4-find (first vall) (l4ntpctxt empty (rest vall) k))]
    [l4arrref (aray posi)
              (l4-find aray (l4aractxt posi k))]
    [l4arrset (aray posi valu)
              (l4-find aray (l4asactxt posi valu k))]
    [l4arrlen (aray)
              (l4-find aray (l4alnctxt k))]
    [l4print  (valu)
              (l4-find valu (l4prtctxt k))]
    [l4makecl (name vars)
              (l4-find vars (l4mclctxt name k))]
    [l4clproc (clos)
              (l4-find clos (l4clpctxt k))]
    [l4clvars (clos)
              (l4-find clos (l4clvctxt k))]
    [l4value  (valu)
              (l4-fill (l3value (l4v-to-l3v valu)) k)]))

(define/contract (l4-fill d k)
  (-> L3Definition? L4Context? L3Expression?)
  (type-case L4Context k
    [l4letctxt (var body k)
               (l3lete (l4v-to-l3v var) d (l4-find body k))]
    [l4ifctxt  (then else k)
               (l4-maybe-lift d (λ (v) (l3ife v (l4-find then k) (l4-find else k))))]
    [l4bilctxt (oper rhs k)
               (l4-maybe-lift d (λ (v) (l4-find rhs (l4birctxt oper v k))))]
    [l4birctxt (oper lv k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3biop oper lv v) k)))]
    [l4prdctxt (oper k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3pred oper v) k)))]
    [l4functxt (argl k)
               (if (l3value? d)
                   (let ([f (unwrap-l3value d)])
                     (if (empty? argl)
                         (l4-fill (l3funcal f empty) k)
                         (l4-find (first argl) (l4argctxt f empty (rest argl) k))))
                   (let ([x (l3varia (var-suffix 'l3_x_ 'L3))])
                     (if (empty? argl)
                         (l3lete x d (l4-fill (l3funcal x empty) k))
                         (l3lete x d (l4-find (first argl) (l4argctxt x empty (rest argl) k))))))]
    [l4argctxt (func done left k)
               (if (l3value? d)
                   (let ([a (unwrap-l3value d)])
                     (if (empty? left)
                         (l4-fill (l3funcal func (reverse (cons a done))) k)
                         (l4-find (first left) (l4argctxt func (cons a done) (rest left) k))))
                   (let ([x (l3varia (var-suffix 'l3_x_ 'L3))])
                     (if (empty? left)
                         (l3lete x d (l4-fill (l3funcal func (reverse (cons x done))) k))
                         (l3lete x d (l4-find (first left) (l4argctxt func (cons x done) (rest left) k))))))]
    [l4nasctxt (valu k)
               (l4-maybe-lift d (λ (v) (l4-find valu (l4navctxt v k))))]
    [l4navctxt (size k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3newarr size v) k)))]
    [l4ntpctxt (done left k)
               (if (empty? left)
                   (l4-maybe-lift d (λ (v) (l4-fill (l3newtup (reverse (cons v done))) k)))
                   (l4-maybe-lift
                    d (λ (v) (l4-find (first left) (l4ntpctxt (cons v done) (rest left) k)))))]
    [l4aractxt (posi k)
               (l4-maybe-lift d (λ (v) (l4-find posi (l4arpctxt v k))))]
    [l4arpctxt (aray k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3arrref aray v) k)))]
    [l4asactxt (posi valu k)
               (l4-maybe-lift d (λ (v) (l4-find posi (l4aspctxt v valu k))))]
    [l4aspctxt (aray valu k)
               (l4-maybe-lift d (λ (v) (l4-find valu (l4asvctxt aray v k))))]
    [l4asvctxt (aray posi k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3arrset aray posi v) k)))]
    [l4alnctxt (k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3arrlen v) k)))]
    [l4prtctxt (k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3printd v) k)))]
    [l4mclctxt (name k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3makecl name v) k)))]
    [l4clpctxt (k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3clproc v) k)))]
    [l4clvctxt (k)
               (l4-maybe-lift d (λ (v) (l4-fill (l3clvars v) k)))]
    [l4noctxt  () (l3defe d)]))
                     
(define/contract (l4-maybe-lift d f)
  (-> L3Definition? (-> L3Value? L3Expression?) L3Expression?)
  (if (l3value? d)
      (let ([v (unwrap-l3value d)])
        (f v))
      (let ([x (l3varia (var-suffix 'l3_x_ 'L3))])
        (l3lete x d (f x)))))