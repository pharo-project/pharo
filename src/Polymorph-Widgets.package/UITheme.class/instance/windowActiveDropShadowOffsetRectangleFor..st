border-styles
windowActiveDropShadowOffsetRectangleFor: aSystemWindow
	"Answer a rectangle describing the offsets for each corner
	of the the active window bounds for the drop shadow area."
	
	^self windowActiveDropShadowStyle == #diffuse
		ifTrue: [self windowDiffuseDropShadowOffsetRectangleFor: aSystemWindow]
		ifFalse: [self windowPlainDropShadowOffsetRectangleFor: aSystemWindow]