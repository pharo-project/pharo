emphasis
emphasis
	"Answer the squeak emphasis code for the receiver.
	1=bold, 2=italic, 3=bold-italic etc"
	| answer |
	answer := 0.
	self isBoldOrBolder ifTrue:[answer := answer + self class squeakWeightBold].
	self isItalicOrOblique ifTrue:[answer := answer + self class squeakSlantItalic].
	^answer