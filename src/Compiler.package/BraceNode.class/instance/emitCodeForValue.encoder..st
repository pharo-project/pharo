code generation
emitCodeForValue: stack encoder: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[elements do: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder