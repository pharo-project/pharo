testing
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self changed