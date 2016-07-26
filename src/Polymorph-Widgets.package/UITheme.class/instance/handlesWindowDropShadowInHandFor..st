border-styles
handlesWindowDropShadowInHandFor: aSystemWindow
	"Answer whether the receiver will handle drop shadow drawing
	for a window when picked up in the hand. We will when diffuse."

	^self windowActiveDropShadowStyle == #diffuse and: [
		aSystemWindow isActive]