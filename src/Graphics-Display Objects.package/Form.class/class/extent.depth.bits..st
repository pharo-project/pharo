instance creation
extent: extentPoint depth: bitsPerPixel bits: aBitmap
	"Answer an instance of me with blank bitmap of the given dimensions and depth."

	^ self basicNew setExtent: extentPoint depth: bitsPerPixel bits: aBitmap