accessing
extent: aPoint
	"Make the choices area at least fill the scroll area."
	^super extent: (aPoint x max: self class minWidth)@(aPoint y).