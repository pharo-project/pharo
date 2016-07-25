accessing
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme sliderNormalFillStyleFor: self]
		ifFalse: [self theme sliderDisabledFillStyleFor: self]