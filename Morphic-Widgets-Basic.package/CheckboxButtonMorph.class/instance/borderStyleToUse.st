accessing
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self isRadioButton
		ifTrue: [self radioBorderStyleToUse]
		ifFalse: [self checkboxBorderStyleToUse]