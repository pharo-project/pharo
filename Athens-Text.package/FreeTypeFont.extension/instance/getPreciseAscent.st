*Athens-Text
getPreciseAscent
	"we using face bouding box for getting ascent value, otherwise two different
	font sizes on same line won't align properly"
	
	^(self face ascender * self pixelSize / self face unitsPerEm) asFloat.
