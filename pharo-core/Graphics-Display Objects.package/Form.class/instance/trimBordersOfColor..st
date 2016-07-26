image manipulation
trimBordersOfColor: aColor
	"Answer a copy of this Form with each edge trimmed in to the first pixel that is not of the given color. (That is, border strips of the given color are removed)."

	| r |
	r := self rectangleEnclosingPixelsNotOfColor: aColor.
	^ self copy: r
