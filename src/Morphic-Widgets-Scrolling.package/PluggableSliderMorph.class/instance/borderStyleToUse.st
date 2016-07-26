protocol
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme sliderNormalBorderStyleFor: self]
		ifFalse: [self theme sliderDisabledBorderStyleFor: self]