code generation
emitCodeForToDo: stack encoder: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt limitInit test block incStmt blockSize |
	initStmt := arguments at: 4.
	limitInit := arguments at: 7.
	test := arguments at: 5.
	block := arguments at: 3.
	incStmt := arguments at: 6.
	blockSize := sizes at: 1.
	loopSize := sizes at: 2.
	limitInit == nil
		ifFalse: [limitInit emitCodeForEffect: stack encoder: encoder].
		
	"This will return the receiver of to:do: which is the initial value of the loop"
	forValue
		ifTrue: [initStmt emitCodeForValue: stack encoder: encoder.]
		ifFalse: [initStmt emitCodeForEffect: stack encoder: encoder].
	test emitCodeForValue: stack encoder: encoder.
	self emitCodeForBranchOn: false dist: blockSize pop: stack encoder: encoder.
	pc := encoder methodStreamPosition.
	block emitCodeForEvaluatedEffect: stack encoder: encoder.
	incStmt emitCodeForEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.