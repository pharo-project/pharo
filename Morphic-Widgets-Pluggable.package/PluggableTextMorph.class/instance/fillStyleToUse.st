accessing
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	self color ifNotNil: [ ^ self color ].
	^self enabled
		ifTrue: [self theme textEditorNormalFillStyleFor: self]
		ifFalse: [self theme textEditorDisabledFillStyleFor: self]