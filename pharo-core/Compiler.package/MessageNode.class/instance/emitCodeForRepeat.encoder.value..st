code generation
emitCodeForRepeat: stack encoder: encoder value: forValue 
	" L1: ... Jmp(L1)"
	| loopSize |
	loopSize := sizes at: 1.
	receiver emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]