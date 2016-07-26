geometry
intersects: aRectangle
	"Answer whether aRectangle, which is in World coordinates, intersects me."

	^self fullBoundsInWorld intersects: aRectangle