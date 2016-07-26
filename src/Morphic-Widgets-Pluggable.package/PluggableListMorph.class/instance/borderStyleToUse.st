geometry
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme listNormalBorderStyleFor: self]
		ifFalse: [self theme listDisabledBorderStyleFor: self]