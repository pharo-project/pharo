*FreeType-Addition
installFreeTypeFont: aFreeTypeFont foregroundColor: foregroundColor backgroundColor: backgroundColor
	"Set up the parameters.  Since the glyphs in a TTCFont is 32bit depth form, it tries to use rule=34 to get better AA result if possible."

	(FreeTypeSettings current useSubPixelAntiAliasing and: [destForm depth >= 8])
		ifTrue:[
			self combinationRule: 41.
			destForm depth = 8
				ifTrue:[self colorMap: (self cachedFontColormapFrom: 32 to: destForm depth)]
				ifFalse:[self colorMap: nil]]
		ifFalse:[
			"use combination rule 34 when rule 41 is not available in the BitBlt plugin,
			or the destination form depth <= 8"
			destForm depth <= 8 
				ifTrue: [
					self colorMap: (self cachedFontColormapFrom: 32 to: destForm depth).
					self combinationRule: Form paint.] 
				ifFalse: [
					self colorMap: nil.
					self combinationRule: 34]].
	halftoneForm := nil.
	sourceX := sourceY := 0.
	height := aFreeTypeFont height.
