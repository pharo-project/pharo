accessing
realTarget
	^ target isSymbol
		ifTrue: [ Smalltalk globals at: target ifAbsent: [  ] ]
		ifFalse: [ target ]