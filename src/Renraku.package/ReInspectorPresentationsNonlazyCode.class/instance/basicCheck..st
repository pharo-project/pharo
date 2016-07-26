running
basicCheck: anAST

	anAST pragmaNamed: #gtInspectorPresentationOrder: ifPresent: [ 
		^ (anAST statements allSatisfy: [ :statement |
			 self
				presentation: anAST arguments first
				isTheLeftmostRecepientIn: (statement isReturn
					ifTrue: [ statement value ]
					ifFalse: [ statement ]) ]) not ].

	^ false