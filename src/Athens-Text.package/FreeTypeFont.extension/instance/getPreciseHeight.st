*Athens-Text
getPreciseHeight
	"get precise font ascent, can be floating value"
	
	^(self face height * self pixelSize / self face unitsPerEm) asFloat.
