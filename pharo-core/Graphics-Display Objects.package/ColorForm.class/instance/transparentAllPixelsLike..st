color manipulation
transparentAllPixelsLike: aPoint
	"Make all occurances of the given pixel value transparent.  Very useful when two entries in the colorMap have the same value.  This only changes ONE."

	self replaceColorAt: aPoint with: Color transparent.
