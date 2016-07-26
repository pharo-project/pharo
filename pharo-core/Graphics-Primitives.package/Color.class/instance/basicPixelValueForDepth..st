conversions
basicPixelValueForDepth: d 
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8, black maps to the darkest possible blue."
	| rgbBlack val |
	d = 8 ifTrue: [ ^ self closestPixelValue8 ].	"common case"
	d < 8 ifTrue: 
		[ d = 4 ifTrue: [ ^ self closestPixelValue4 ].
		d = 2 ifTrue: [ ^ self closestPixelValue2 ].
		d = 1 ifTrue: [ ^ self closestPixelValue1 ] ].
	rgbBlack := 1.	"closest black that is not transparent in RGB"
	d = 16 ifTrue: 
		[ "five bits per component; top bits ignored"
		val := (((rgb bitShift: -15) bitAnd: 31744) bitOr: ((rgb bitShift: -10) bitAnd: 992)) bitOr: ((rgb bitShift: -5) bitAnd: 31).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	d = 32 ifTrue: 
		[ "eight bits per component; top 8 bits set to all ones (opaque alpha)"
		val := LargePositiveInteger new: 4.
		val 
			at: 3
			put: ((rgb bitShift: -22) bitAnd: 255).
		val 
			at: 2
			put: ((rgb bitShift: -12) bitAnd: 255).
		val 
			at: 1
			put: ((rgb bitShift: -2) bitAnd: 255).
		val 
			at: 4
			put: 255.	"opaque alpha"
		^ val ].
	d = 12 ifTrue: 
		[ "for indexing a color map with 4 bits per color component"
		val := (((rgb bitShift: -18) bitAnd: 3840) bitOr: ((rgb bitShift: -12) bitAnd: 240)) bitOr: ((rgb bitShift: -6) bitAnd: 15).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	d = 9 ifTrue: 
		[ "for indexing a color map with 3 bits per color component"
		val := (((rgb bitShift: -21) bitAnd: 448) bitOr: ((rgb bitShift: -14) bitAnd: 56)) bitOr: ((rgb bitShift: -7) bitAnd: 7).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	self error: 'unknown pixel depth: ' , d printString