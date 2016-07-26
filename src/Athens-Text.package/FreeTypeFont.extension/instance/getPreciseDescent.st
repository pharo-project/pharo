*Athens-Text
getPreciseDescent
	"we using face bouding box for getting ascent value, otherwise two different
	font sizes on same line won't align properly"
	
	^(self face descender * self pixelSize / self face unitsPerEm) asFloat negated.

