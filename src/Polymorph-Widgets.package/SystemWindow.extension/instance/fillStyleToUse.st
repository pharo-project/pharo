*Polymorph-Widgets
fillStyleToUse
	"Answer the basic fill style for the receiver."

	^self isActive 
		ifTrue: [self activeFillStyle] 
		ifFalse: [self inactiveFillStyle]