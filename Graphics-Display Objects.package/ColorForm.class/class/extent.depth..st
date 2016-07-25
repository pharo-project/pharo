instance creation
extent: extentPoint depth: bitsPerPixel
	"Answer an instance of me with blank bitmap of the given dimensions and depth max 8."
	
	^ bitsPerPixel > 8 
		ifTrue: [ self basicNew setExtent: extentPoint depth: 8]
		ifFalse:  [ self basicNew setExtent: extentPoint depth: bitsPerPixel]
