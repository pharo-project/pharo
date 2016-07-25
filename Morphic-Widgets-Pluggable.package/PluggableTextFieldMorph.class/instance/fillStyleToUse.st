accessing
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	self color ifNotNil: [ ^ self color ].
	^self enabled
		ifTrue: [self theme textFieldNormalFillStyleFor: self]
		ifFalse: [self theme textFieldDisabledFillStyleFor: self]