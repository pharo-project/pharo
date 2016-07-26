*Graphics-Primitives
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap