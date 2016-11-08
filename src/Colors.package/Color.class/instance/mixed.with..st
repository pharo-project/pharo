transformations
mixed: proportion with: aColor 
	"Mix with another color and do not preserve transpareny.  Only use this for extracting the RGB value and mixing it.  All other callers should use instead: 
	aColor alphaMixed: proportion with: anotherColor
	"
	| frac1 frac2 |
	frac1 := proportion asFloat 
		min: 1.0
		max: 0.0.
	frac2 := 1.0 - frac1.
	^ self class 
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)