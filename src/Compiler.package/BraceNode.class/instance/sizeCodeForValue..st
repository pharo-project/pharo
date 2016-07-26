code generation
sizeCodeForValue: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				MessageNode new
					receiver: (encoder encodeVariable: #Array)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				CascadeNode new
					receiver: (MessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | MessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (MessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder