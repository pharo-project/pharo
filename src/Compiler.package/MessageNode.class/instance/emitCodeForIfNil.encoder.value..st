code generation
emitCodeForIfNil: stack encoder: encoder value: forValue

	| theNode theSize ifNotNilSelector |
	theNode := arguments first.
	theSize := sizes at: 1.
	ifNotNilSelector := #ifNotNil:.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue ifTrue: [encoder genDup. stack push: 1].
	encoder genPushSpecialLiteral: nil. stack push: 1.
	equalNode emitCode: stack args: 1 encoder: encoder.
	self 
		emitCodeForBranchOn: (selector key == ifNotNilSelector)
		dist: theSize 
		pop: stack 
		encoder: encoder.
	pc := encoder methodStreamPosition.
	forValue 
		ifTrue: 
			[encoder genPop. stack pop: 1.
			theNode emitCodeForEvaluatedValue: stack encoder: encoder]	
		ifFalse: [theNode emitCodeForEvaluatedEffect: stack encoder: encoder]