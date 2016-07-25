color mapping
balancedPatternFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	self hasNonStandardPalette
		ifTrue:[ ^ self bitPatternFor: aColor]
		ifFalse:[^ self balancedPatternFor: aColor depth: self depth]