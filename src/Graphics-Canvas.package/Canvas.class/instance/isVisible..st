testing
isVisible: aRectangle
	"Return true if the given rectangle is (partially) visible"
	^self clipRect intersects: aRectangle
