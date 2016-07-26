instance creation
extent: extentPoint depth: bitsPerPixel
	"Answer an instance of me with blank bitmap of the given dimensions and depth."

	^ self basicNew setExtent: extentPoint depth: bitsPerPixel
