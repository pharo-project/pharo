geometry
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme listNormalFillStyleFor: self]
		ifFalse: [self theme listDisabledFillStyleFor: self]