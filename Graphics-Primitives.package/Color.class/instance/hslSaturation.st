accessing
hslSaturation
	"Return the HSL saturation of this color, a value between 0.0 and 1.0."

	| chroma |
	chroma := self chroma.
	"Chroma is zero for black and white, which are the two cases of division by zero below."
	chroma isZero ifTrue: [ ^ chroma ].
	
	^ chroma / (1.0 - (self lightness * 2.0 - 1.0) abs)