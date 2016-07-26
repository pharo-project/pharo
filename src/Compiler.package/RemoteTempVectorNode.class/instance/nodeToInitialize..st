code generation (closures)
nodeToInitialize: encoder
	^AssignmentNode new
		variable: self
		value: (encoder supportsClosureOpcodes
					ifTrue: [NewArrayNode new numElements: remoteTemps size]
					ifFalse:
						[MessageNode new
							receiver: (encoder encodeVariable: 'Array')
							selector: #new:
							arguments: (Array with: (encoder encodeLiteral: remoteTemps size))
							precedence: 3
							from: encoder])