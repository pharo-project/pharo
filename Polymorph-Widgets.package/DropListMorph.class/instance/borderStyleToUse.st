private
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme dropListNormalBorderStyleFor: self]
		ifFalse: [self theme dropListDisabledBorderStyleFor: self]