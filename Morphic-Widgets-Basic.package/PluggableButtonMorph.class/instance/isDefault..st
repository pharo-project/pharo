accessing
isDefault: aBoolean
	"Set whether the button is to be considered default."

	aBoolean
		ifTrue: [self setProperty: #isDefault toValue: true]
		ifFalse: [self removeProperty: #isDefault].
	self changed