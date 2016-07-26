testing
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray]).
