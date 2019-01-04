	adc r2, r3
	add r2, r3
	adiw r26, 3
	and r16, r17
	andi r16,0
	asr r31
	bclr 4
	bld 25, 7
	a:
	brbc 4,a
	b:
	brbs 4,a
	c:
	brcc c
	d:
	brcs d
	break
	e:
	breq e
	f:
	brge f
	g:
	brhc g
	h:
	brhs h
	i:
	brid i
	j:
	brie j
	k:
	brlo k
	l:
	brlt l
	m:
	brmi m
	n:
	brne n
	o:
	brpl o
	p:
	brsh p
	q:
	brtc q
	r:
	brts r
	s:
	brvc s
	t:
	brvs t
	bset 5
	bst R20, 6
	call 2
	cbi 30, 7
	clc
	clh
	cli
	cln
	clr r10
	cls
	clt
	clv
	clz
	com R11
	cp r10, r21
	cpc r10, r21
	cpi r16, 20
	cpse r31, r30
	dec r29
	  ;;  des
	eicall
	eijmp
	elpm
	  elpm r15,Z
	  elpm r16,Z+
	  eor r13, r14
	  fmul r17, r18
	  fmuls r17, r18
	 	fmulsu r17, r18
	  icall
	  ijmp
	  in r10, 62
	  inc r20
	  jmp 2
	  lac Z, R10
	  las Z, R10
	 	lat Z, R10
	  ld R10, X
	  ld R10, X+
	  ld R10, -X
	  ld R10, Y
	 	ld R10, Y+
	 	ld R10, -Y
	  ldd R10, Y+3
	  	ld R10, Z
	 	ld R10, Z+
	 	ld R10, -Z
	  ldd R10, Z+3
	  ldi r19, 255
	  lds r21, 65534
  lpm
  lpm R10, Z
  lpm R10, Z+
  lsl R18
  lsr r18
  mov r10, r20
  movw r16, r20
  mul r20, r31
  muls r18, r19
  mulsu r18, r19
  neg r16
  nop
  or r10, r11
  ori r16, 255
  out 60, R10
  pop r10
  push r10
y:
  rcall z
  ret
  reti
z:
  rjmp z
  ror R18
  sbc r10, r11
  sbci R20, 210
  sbi 20, 7
  sbic 20, 7
  sbis 20, 7
  sbiw 26, 30
  sbr r20, 241
  sbrc R10, 6
 	sbrs R10, 6
  sec
  seh
  sei
  sen
  ser R20
  ses
  set
  sev
  sez
  sleep
  spm
  spm z+
	  	st X, R10
	  st X+, R10
	  st -X, R10
	  st Y, R10
	 	st 	Y+, R10
	 	st 	-Y, R10
	  std 	Y+3, R10
	  	st Z, R10
	 	st 	Z+, R10
	 	st 	-Z, R10
	  std 	Z+3, R10
  sts 65534, r21

  sub r10, r11
  subi r16, 22
  swap r15
  wdr
  xch z, r22
