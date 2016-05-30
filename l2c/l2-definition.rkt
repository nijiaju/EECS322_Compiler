#lang plai

(struct prog (name funl))

(struct func (name narg nspl insl))

(define-type Inst
  [numbr (numb number?)]
  [regst (regs symbol?)]
  [label (labl symbol?)]
  [varia (vari symbol?)]
  [loadi (sorc Inst?) (offs number?)]
  [stack (stak number?)]
  [movei (dest Inst?) (sorc Inst?)]
  [aropi (oper Aop?) (dest Inst?) (sorc Inst?)]
  [sfopi (oper Sop?) (dest Inst?) (sorc Inst?)]
  [compi (comp Cmp?) (dest Inst?) (lhs  Inst?) (rhs  Inst?)]
  [cjmpi (comp Cmp?) (lhs  Inst?) (rhs  Inst?) (true Inst?) (fals Inst?)]
  [gotoi (labl Inst?)]
  [calli (dest Inst?) (narg number?)]
  [tcall (dest Inst?) (narg number?)]
  [cread]
  [cprit]
  [caloc]
  [caerr]
  [retun])

(define-type Aop
  [addop]
  [subop]
  [mulop]
  [andop])

(define-type Sop
  [sflft]
  [sfrht])

(define-type Cmp
  [less]
  [leeq]
  [eqal])

(struct killgen (kill gen))

(struct inout (in out))