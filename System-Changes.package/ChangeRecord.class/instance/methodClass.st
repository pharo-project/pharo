access
methodClass
	| methodClass |
	type == #method
		ifFalse: [ ^ nil ].
	(Smalltalk globals includesKey: class asSymbol)
		ifFalse: [ ^ nil ].
	methodClass := Smalltalk globals at: class asSymbol.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]