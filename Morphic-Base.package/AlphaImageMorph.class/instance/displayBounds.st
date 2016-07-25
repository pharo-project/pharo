geometry
displayBounds
	"Answer a rectangle in display coordinates that
	bounds the image (may be larger/smaller than visible area).
	Just one rep for the tiled case."

	^self layoutPosition extent: self cachedForm extent
	