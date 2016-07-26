conversions
pixelWordForDepth: depth 
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| pixelValue basicPixelWord |
	self isTransparent ifTrue: [ ^ 0 ].
	pixelValue := self pixelValueForDepth: depth.
	basicPixelWord := self pixelWordFor: depth filledWith: pixelValue.
	^ depth < 32 
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16777215) bitOr: (alpha bitShift: 24) ]