accessing
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme rubScrolledTextMorphThemer normalBorderStyleFor: self]
		ifFalse: [self theme rubScrolledTextMorphThemer disabledBorderStyleFor: self]