displaying
installOn: aBitBlt foregroundColor: foreColor backgroundColor: backColor

	| |
	"fcolor := foreColor pixelValueForDepth: 32."
	aBitBlt installFreeTypeFont: self foregroundColor: foreColor backgroundColor: backColor.
