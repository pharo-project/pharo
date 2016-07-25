code generation
sizeCodeForToDo: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := Array with: blockSize with: loopSize.
	^initSize
	+ loopSize