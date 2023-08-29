Extension { #name : #GrafPort }

{ #category : #'*FreeType-Graphics' }
GrafPort >> installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [

	super installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale.
	lastFont := aFreeTypeFont.
	lastFontForegroundColor := foregroundColor.
	lastFontBackgroundColor := backgroundColor
]
