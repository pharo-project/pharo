border-styles
windowPlainDropShadowOffsetRectangleFor: aSystemWindow
	"Answer a rectangle describing the offsets for each corner
	of the the active window bounds for the drop shadow area
	when the plain style is used."
	
	^aSystemWindow shadowOffset negated corner: aSystemWindow shadowOffset