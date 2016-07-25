accessing
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	
	^self isRadioButton
		ifTrue: [self radioFillStyleToUse]
		ifFalse: [self checkboxFillStyleToUse]