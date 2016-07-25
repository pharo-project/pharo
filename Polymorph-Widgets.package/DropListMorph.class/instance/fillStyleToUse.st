private
fillStyleToUse
	"Answer the fillStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme dropListNormalFillStyleFor: self]
		ifFalse: [self theme dropListDisabledFillStyleFor: self]