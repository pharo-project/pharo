*FreeType-addition
installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor

	super installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor.
	lastFont := aFreeTypeFont.
	lastFontForegroundColor := foregroundColor.
	lastFontBackgroundColor := backgroundColor.
