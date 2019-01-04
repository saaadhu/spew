#lang racket
(require threading)
(require racket/string)
(require data/bit-vector)
(define operandclass (list 'src 'dst))
(struct operand (char mask type klass))
(define operandtypes (list 'reg 'imm 'io-reg 'const-data 'status-bit 'register-bit 'const-address))
(define insnclass (list 'al 'branch 'data-transfer 'bit 'mcu-control 'nop))
(struct insn (syntx klass mask opcode operands operand-manip))

(define (make-mask bits)
  (~> bits
       list->bit-vector
       bit-vector->string
       (string->number 2)))

(define (make-insn-mask bits)
  (make-mask (map (lambda (c) (char-numeric? c)) bits)))
(define (make-insn-opcode bits)
  (make-mask (map (lambda (c) (char=? c #\1)) bits)))

(define (make-operand bits char)
  (operand
   char
   (list->bit-vector (map (lambda (c) (char=? c char)) bits))
   (cond
     [(char=? char #\d) 'reg]
     [(char=? char #\r) 'reg]
     [(char=? char #\A) 'io-reg]
     [(char=? char #\K) 'const-data]
     [(char=? char #\s) 'status-bit]
     [(char=? char #\b) 'register-bit]
     [(char=? char #\k) 'const-address]
     )
   (cond
     [(char=? char #\d) 'dst]
     [(char=? char #\r) 'src]
     [(char=? char #\K) 'src]
     [(char=? char #\s) 'dst]
     )))

(define (extract-operand operand data)
  (let* ([mask (operand-mask operand)]
        [mask-length (bit-vector-length mask)])
    (make-mask
     (reverse
      (for/list
          ([i (in-range mask-length)]
           #:when (bit-vector-ref mask (- mask-length i 1)))
           (bitwise-bit-set? data i))))))

(define (make-operands bits)
  (~>> bits
       (filter (lambda (c) (not (char-numeric? c))))
       remove-duplicates
       (map (lambda (c) (make-operand bits c)))))

(define (make-insn syntx insn-class opcode [operand-manip (lambda (op-char val) val)])
  (let* ((bits (filter (lambda (c) (not (char-whitespace? c))) (string->list opcode)))
         (insn-mask (make-insn-mask bits))
         [insn-opcode (make-insn-opcode bits)]
         (operands (make-operands bits)))
    (insn syntx insn-class insn-mask insn-opcode operands operand-manip)))

(define (match-opcode insn data)
  (= (bitwise-and (insn-mask insn) data) (insn-opcode insn)))

(define (print-insn insn data)
  (let* ([parts (string-split (insn-syntx insn))]
         [name (first parts)])
  (printf "~a ~a~n"
          name
          (foldl
           (lambda (op str)
             (string-replace
              str
              (string (operand-char op))
              (number->string ((insn-operand-manip insn) op (extract-operand op data)))))
           (if (> (length parts) 1) (second parts) "")
           (insn-operands insn))
;           (string-join (map (lambda (op) (string-append "Op " (string (operand-char op)) " " (bit-vector->string (operand-mask op)))) (insn-operands insn)))
           )))

(define (disassemble-word data)
  (let* [(insns (filter (lambda (insn) (match-opcode insn data)) insns))
         (matched (not (empty? insns)))]
    (when matched
      (print-insn (first insns) data))
    matched))

(define (disassemble data)
  (unless (empty? data)
    (if (disassemble-word (bitwise-ior (arithmetic-shift (first (rest data)) 8) (first data)))
        (disassemble (rest (rest data)))
        (if (disassemble-word (bitwise-ior
                               (arithmetic-shift (second data) 24)
                               (arithmetic-shift (first data) 16)
                               (arithmetic-shift (fourth data) 8)
                               (third data)))
            (disassemble (rest (rest (rest (rest data)))))
            (printf "ERROR: Unknown insn starting at ~a~n" data)))))

(define reg-pair (lambda (op val) (if (symbol=? (operand-type op) 'reg) (+ 24 (* val 2)) val)))
(define reg-pair-all (lambda (op val) (if (symbol=? (operand-type op) 'reg) (* val 2) val)))
(define reg-upper (lambda (op val) (if (symbol=? (operand-type op) 'reg) (+ 16 val) val)))

(define insns
  (list
   (make-insn "ADC  Rd,Rr"       'arith         "0001 11rd dddd rrrr")
   (make-insn "ADD  Rd,Rr"       'arith         "0000 11rd dddd rrrr")
   (make-insn "ADIW Rd,K"       'arith          "1001 0110 KKdd KKKK" reg-pair)
   (make-insn "AND  Rd,Rr"       'arith         "0010 00rd dddd rrrr")
   (make-insn "ANDI Rd,K"       'arith          "0111 KKKK dddd KKKK" reg-upper)
   (make-insn "ASR  Rd"         'arith          "1001 010d dddd 0101")
   ;(make-insn "BCLR s"           'bit           "1001 0100 1sss 1000")
   (make-insn "BLD  Rd,b"         'bit          "1111 100d dddd 0bbb")
   (make-insn "BRCC k"         'branch          "1111 01kk kkkk k000")
   (make-insn "BRCS k"         'branch          "1111 00kk kkkk k000")
   (make-insn "BREQ k"         'branch          "1111 00kk kkkk k001")
   (make-insn "BRGE k"         'branch          "1111 01kk kkkk k100")
   (make-insn "BRHC k"         'branch          "1111 01kk kkkk k101")
   (make-insn "BRHS k"         'branch          "1111 00kk kkkk k101")
   (make-insn "BRID k"         'branch          "1111 01kk kkkk k111")
   (make-insn "BRIE k"         'branch          "1111 00kk kkkk k111")
   (make-insn "BRLO k"         'branch          "1111 00kk kkkk k000")
   (make-insn "BRLT k"         'branch          "1111 00kk kkkk k100")
   (make-insn "BRMI k"         'branch          "1111 00kk kkkk k010")
   (make-insn "BRNE k"         'branch          "1111 01kk kkkk k001")
   (make-insn "BRPL k"         'branch          "1111 01kk kkkk k010")
   (make-insn "BRSH k"         'branch          "1111 01kk kkkk k000")
   (make-insn "BRTC k"         'branch          "1111 01kk kkkk k110")
   (make-insn "BRTS k"         'branch          "1111 00kk kkkk k110")
   (make-insn "BRVC k"         'branch          "1111 01kk kkkk k011")
   (make-insn "BRVS k"         'branch          "1111 00kk kkkk k011")
  ; (make-insn "BSET s"         'bit             "1001 0100 0sss 1000")
   (make-insn "BST  Rd,b"      'bit             "1111 101d dddd 0bbb")
   (make-insn "CALL k"         'branch-call     "1001 010k kkkk 111k kkkk kkkk kkkk kkkk")
   (make-insn "CBI  A,b"        'bit            "1001 1000 AAAA Abbb")
;  (make-insn "CBR  Rd,K"       'bit            "1001 1000 AAAA Abbb") same as ANDI
   (make-insn "CLC"             'bit            "1001 0100 1000 1000")
   (make-insn "CLH"             'bit            "1001 0100 1101 1000")
   (make-insn "CLI"             'bit            "1001 0100 1111 1000")
   (make-insn "CLN"             'bit            "1001 0100 1010 1000")
;   (make-insn "CLR Rd"          'bit           "0010 01dd dddd dddd") same as EOR Rd,Rd
   (make-insn "CLS"             'bit            "1001 0100 1100 1000")
   (make-insn "CLT"             'bit            "1001 0100 1110 1000")
   (make-insn "CLV"             'bit            "1001 0100 1011 1000")
   (make-insn "CLZ"             'bit            "1001 0100 1001 1000")
   (make-insn "COM  Rd"         'bit            "1001 010d dddd 0000")
   (make-insn "CP   Rd,Rr"       'compare       "0001 01rd dddd rrrr")
   (make-insn "CPC  Rd,Rr"      'compare        "0000 01rd dddd rrrr")
   (make-insn "CPI  Rd,K"       'compare        "0011 KKKK dddd KKKK" reg-upper)
   (make-insn "CPSE Rd,Rr"      'branch-skip    "0001 00rd dddd rrrr")
   (make-insn "DEC  Rd"         'arith          "1001 010d dddd 1010")
   (make-insn "DES  K"          'arith          "1001 0100 KKKK 1011")
   (make-insn "EICALL"          'branch-call    "1001 0101 0001 1001")
   (make-insn "EIJMP"           'branch         "1001 0100 0001 1001")
   (make-insn "ELPM"            'data-transfer  "1001 0101 1101 1000")
   (make-insn "ELPM Rd,Z"      'data-transfer   "1001 000d dddd 0110")
   (make-insn "ELPM Rd,Z+"     'data-transfer   "1001 000d dddd 0111")
   (make-insn "EOR  Rd,Rr"      'arith          "0010 01rd dddd rrrr")
   (make-insn "FMUL Rd,Rr"      'arith          "0000 0011 0ddd 1rrr" reg-upper)
   (make-insn "FMULS Rd,Rr"     'arith          "0000 0011 1ddd 0rrr" reg-upper)
   (make-insn "FMULSU Rd,Rr"     'arith         "0000 0011 1ddd 1rrr" reg-upper)
   (make-insn "ICALL"           'branch-call    "1001 0101 0000 1001")
   (make-insn "IJMP"            'branch-call    "1001 0100 0000 1001")
   (make-insn "IN Rd,A"         'data-transfer  "1011 0AAd dddd AAAA")
   (make-insn "INC  Rd"         'arith          "1001 010d dddd 0011")
   (make-insn "JMP k"           'branch         "1001 010k kkkk 110k kkkk kkkk kkkk kkkk")
   (make-insn "LAC Z,Rr"        'data-transfer  "1001 001r rrrr 0110")
   (make-insn "LAS Z,Rr"        'data-transfer  "1001 001r rrrr 0101")
   (make-insn "LAT Z,Rr"        'data-transfer  "1001 001r rrrr 0111")
   (make-insn "LD Rd,X"         'data-transfer  "1001 000d dddd 1100")
   (make-insn "LD Rd,X+"        'data-transfer  "1001 000d dddd 1101")
   (make-insn "LD Rd,-X"        'data-transfer  "1001 000d dddd 1110")
   (make-insn "LD Rd,Y"         'data-transfer  "1000 000d dddd 1000")
   (make-insn "LD Rd,Y+"        'data-transfer  "1001 000d dddd 1001")
   (make-insn "LD Rd,-Y"        'data-transfer  "1001 000d dddd 1010")
   (make-insn "LD Rd,Y+q"       'data-transfer  "10q0 qq0d dddd 1qqq")
   (make-insn "LD Rd,Z"         'data-transfer  "1000 000d dddd 0000")
   (make-insn "LD Rd,Z+"        'data-transfer  "1001 000d dddd 0001")
   (make-insn "LD Rd,-Z"        'data-transfer  "1001 000d dddd 0010")
   (make-insn "LD Rd,Z+q"       'data-transfer  "10q0 qq0d dddd 0qqq")
   (make-insn "LDI Rd,K"        'data-transfer  "1110 KKKK dddd KKKK" reg-upper)
   (make-insn "LDS Rd,k"        'data-transfer  "1001 000d dddd 0000 kkkk kkkk kkkk kkkk")
   (make-insn "LDS Rd,k"        'data-transfer  "1010 0kkk dddd kkkk" reg-upper)
   (make-insn "LPM"             'data-transfer  "1001 0101 1100 1000")
   (make-insn "LPM Rd,Z"        'data-transfer  "1001 000d dddd 0100")
   (make-insn "LPM Rd,Z+"       'data-transfer  "1001 000d dddd 0101")
 ; (make-insn "LSL  Rd"         'arith          "0000 11dd dddd dddd") same as ADD Rd, Rd
   (make-insn "LSR  Rd"         'arith          "1001 010d dddd 0110")
   (make-insn "MOV Rd,Rr"       'data-transfer  "0010 11rd dddd rrrr")
   (make-insn "MOVW Rd,Rr"      'data-transfer  "0000 0001 dddd rrrr" reg-pair-all)
   (make-insn "MUL  Rd,Rr"      'arith          "1001 11rd dddd rrrr")
   (make-insn "MULS  Rd,Rr"     'arith          "0000 0010 dddd rrrr" reg-upper)
   (make-insn "MULSU Rd,Rr"     'arith          "0000 0011 0ddd 0rrr" reg-upper)
   (make-insn "NEG Rd"          'arith          "1001 010d dddd 0001")
   (make-insn "NOP"             'nop            "0000 0000 0000 0000")
   (make-insn "OR Rd,Rr"        'arith          "0010 10rd dddd rrrr")
   (make-insn "ORI Rd,K"        'arith          "0110 KKKK dddd KKKK" reg-upper)
   (make-insn "OUT A,Rr"        'data-transfer  "1011 1AAr rrrr AAAA")
   (make-insn "POP Rd"          'data-transfer  "1001 000d dddd 1111")
   (make-insn "PUSH Rr"         'data-transfer  "1001 001r rrrr 1111")
   (make-insn "RCALL k"         'branch-call    "1101 kkkk kkkk kkkk")
   (make-insn "RET"             'branch-call    "1001 0101 0000 1000")
   (make-insn "RETI"            'branch-call    "1001 0101 0001 1000")
   (make-insn "RJMP k"          'branch         "1100 kkkk kkkk kkkk")
 ; (make-insn "ROL  Rd"         'arith          "0001 11dd dddd dddd") same as ADC Rd, Rd
   (make-insn "ROR  Rd"         'arith          "1001 010d dddd 0111")
   (make-insn "SBC  Rd,Rr"       'arith         "0000 10rd dddd rrrr")
   (make-insn "SBCI Rd,K"       'arith          "0100 KKKK dddd KKKK" reg-upper)
   (make-insn "SBI  A,b"         'bit           "1001 1010 AAAA Abbb")
   (make-insn "SBIC A,b"         'branch-skip   "1001 1001 AAAA Abbb")
   (make-insn "SBIS A,b"         'branch-skip   "1001 1011 AAAA Abbb")
   (make-insn "SBIW Rd,K"       'arith          "1001 0111 KKdd KKKK" reg-pair)
 ;  (make-insn "SBR  Rd,K"         'bit          "0110 KKKK dddd KKKK" reg-upper) same as ORI Rd, K
   (make-insn "SBRC Rr,b"         'branch-skip   "1111 110r rrrr 0bbb")
   (make-insn "SBRS Rr,b"         'branch-skip   "1111 111r rrrr 0bbb")
   (make-insn "SEC"             'bit            "1001 0100 0000 1000")
   (make-insn "SEH"             'bit            "1001 0100 0101 1000")
   (make-insn "SEI"             'bit            "1001 0100 0111 1000")
   (make-insn "SEN"             'bit            "1001 0100 0010 1000")
 ;  (make-insn "SER Rd"          'bit            "1110 1111 dddd 1111") same as LDI Rd, 255
   (make-insn "SES"             'bit            "1001 0100 0100 1000")
   (make-insn "SET"             'bit            "1001 0100 0110 1000")
   (make-insn "SEV"             'bit            "1001 0100 0011 1000")
   (make-insn "SEZ"             'bit            "1001 0100 0001 1000")
   (make-insn "SPM"             'data-transfer  "1001 0101 1110 1000")
   (make-insn "SPM Z+"          'data-transfer  "1001 0101 1111 1000")
   (make-insn "BRBC s,k"         'branch        "1111 01kk kkkk ksss")
   (make-insn "ST X,Rr"         'data-transfer  "1001 001r rrrr 1100")
   (make-insn "ST X+,Rr"        'data-transfer  "1001 001r rrrr 1101")
   (make-insn "ST -X,Rr"        'data-transfer  "1001 001r rrrr 1110")
   (make-insn "ST Y,Rr"         'data-transfer  "1000 001r rrrr 1000")
   (make-insn "ST Y+,Rr"        'data-transfer  "1001 001r rrrr 1001")
   (make-insn "ST -Y,Rr"        'data-transfer  "1001 001r rrrr 1010")
   (make-insn "ST Y+q,Rr"       'data-transfer  "10q0 qq1r rrrr 1qqq")
   (make-insn "ST Z,Rr"         'data-transfer  "1000 001r rrrr 0000")
   (make-insn "ST Z+,Rr"        'data-transfer  "1001 001r rrrr 0001")
   (make-insn "ST -Z,Rr"        'data-transfer  "1001 001r rrrr 0010")
   (make-insn "ST Z+q,Rr"       'data-transfer  "10q0 qq1r rrrr 0qqq")
   (make-insn "STS Rd,k"        'data-transfer  "1001 001d dddd 0000 kkkk kkkk kkkk kkkk")
   (make-insn "STS Rd,k"        'data-transfer  "1010 1kkk dddd kkkk" reg-upper)
   (make-insn "SUB  Rd,Rr"      'arith          "0001 10rd dddd rrrr")
   (make-insn "SUBI Rd,K"       'arith          "0101 KKKK dddd KKKK" reg-upper)
   (make-insn "SWAP Rd"         'arith          "1001 010d dddd 0010")
  ; (make-insn "TST Rd"          'arith          "0010 00dd dddd dddd") same as AND Rd, Rd
   (make-insn "XCH Z,Rr"        'data-transfer  "1001 001r rrrr 0100")
   (make-insn "BRBS s,k"         'branch        "1111 00kk kkkk ksss")
   (make-insn "BREAK"         'mcu-control      "1001 0101 1001 1000")
   (make-insn "SLEEP"         'mcu-control      "1001 0101 1000 1000")
   (make-insn "WDR"           'mcu-control      "1001 0101 1010 1000")
   ))

(define all-avr-insns-data '[#x23 #x1c #x23 #x0c #x13 #x96 #x01 #x23 #xff #x7f #xf5 #x95 #xc8 #x94 #x97 #xf9 #x04 #xf4 #x04 #xf0 #x00 #xf4 #x00 #xf0 #x98 #x95 #x01 #xf0 #x04 #xf4 #x05 #xf4 #x05 #xf0 #x07 #xf4 #x07 #xf0 #x00 #xf0 #x00 #xf4 #x02 #xf0 #x01 #xf4 #x02 #xf4 #x00 #xf4 #x06 #xf4 #x06 #xf0 #x03 #xf4 #x03 #xf0 #x58 #x94 #x46 #xfb #x0e #x94 #x01 #x00 #xf7 #x98 #x88 #x94 #xd8 #x94 #xf8 #x94 #xa8 #x94 #xc8 #x94 #xe8 #x94 #xb8 #x94 #x98 #x94 #xb0 #x94 #xa5 #x16 #xa5 #x06 #x04 #x31 #xfe #x13 #xda #x95 #x19 #x95 #x19 #x94 #xd8 #x95 #xf6 #x90 #x07 #x91 #xde #x24 #x1a #x03 #x92 #x03 #x9a #x03 #x09 #x95 #x09 #x94 #xae #xb6 #x43 #x95 #x0c #x94 #x01 #x00 #xa6 #x92 #xa5 #x92 #xa7 #x92 #xac #x90 #xad #x90 #xae #x90 #xa8 #x80 #xa9 #x90 #xaa #x90 #xab #x80 #xa0 #x80 #xa1 #x90 #xa2 #x90 #xa3 #x80 #x3f #xef #x50 #x91 #xfe #xff #x18 #xa7 #xc8 #x95 #xa4 #x90 #xa5 #x90 #x22 #x0f #x26 #x95 #xa4 #x2e #x8a #x01 #x4f #x9f #x23 #x02 #x23 #x03 #x01 #x95 #x00 #x00 #xab #x28 #x0f #x6f #xac #xbe #xaf #x90 #xaf #x92 #x00 #xd0 #x08 #x95 #x18 #x95 #x00 #xc0 #x27 #x95 #xab #x08 #x42 #x4d #xa7 #x9a #xa7 #x99 #xa7 #x9b #x5e #x97 #x41 #x6f #xa6 #xfc #xa6 #xfe #x08 #x94 #x58 #x94 #x78 #x94 #x28 #x94 #x4f #xef #x48 #x94 #x68 #x94 #x38 #x94 #x18 #x94 #x88 #x95 #xe8 #x95 #xf8 #x95 #xac #x92 #xad #x92 #xae #x92 #xa8 #x82 #xa9 #x92 #xaa #x92 #xab #x82 #xa0 #x82 #xa1 #x92 #xa2 #x92 #xa3 #x82 #x50 #x93 #xfe #xff #xab #x18 #x06 #x51 #xf2 #x94 #xa8 #x95 #x64 #x93])
