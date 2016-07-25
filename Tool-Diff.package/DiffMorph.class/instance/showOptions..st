accessing
showOptions: aBoolean

	showOptions = aBoolean ifTrue: [ ^ self ].
	showOptions := aBoolean.
	aBoolean
		ifTrue: [ self addOptionsPanel ]
		ifFalse: [ self removeOptionsPanel ]