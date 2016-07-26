instance initialization
signature: signature "<String>" block: aBlock "<BlockClosure>"
	| method |
	method := self evaluatorForSignature: signature.
	block := aBlock.
	thunk := (method pragmaAt: #calleepops:)
				ifNil: [FFICallbackThunk newCCall]
				ifNotNil:
					[:pragma|
					FFICallbackThunk newStdCall: (pragma argumentAt: 1)].
	numEvaluatorArgs := (evaluator := method selector) numArgs.
	self addToThunkTable