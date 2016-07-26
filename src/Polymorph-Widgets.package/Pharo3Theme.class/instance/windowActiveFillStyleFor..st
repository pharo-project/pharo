fill-styles
windowActiveFillStyleFor: aWindow
	"We do not want the lighting effect when the window goes inactive"
	^SolidFillStyle color: self class baseColor