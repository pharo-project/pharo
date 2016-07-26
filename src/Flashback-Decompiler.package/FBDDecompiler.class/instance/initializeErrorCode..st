initialize
initializeErrorCode: cm
	| index |
	cm isPrimitive
		ifFalse: [ ^ self ].
	instructionStream willStore
		ifFalse: [ ^ self ].
	index := instructionStream nextInstruction arguments first.
	currentSequence removeTemporaryNamed: (simulatedStack at: index + 1) name.
	simulatedStack at: index + 1 put: (builder codeTemp: (self errorCodeNameFor: cm))