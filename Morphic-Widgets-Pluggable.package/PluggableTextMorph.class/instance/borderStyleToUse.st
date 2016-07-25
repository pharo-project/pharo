accessing
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme textEditorNormalBorderStyleFor: self]
		ifFalse: [self theme textEditorDisabledBorderStyleFor: self]