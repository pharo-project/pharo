displaying
displayLineGlyphOn: aDisplayContext from: startPoint to: endPoint
	|  oldCombinationRule oldHalftoneForm originalColorMap clr depth foreColorVal foreColorAlpha glyph width height
	startPointX startPointY endPointX endPointY foreColor |
	oldCombinationRule := aDisplayContext combinationRule .
	oldHalftoneForm := aDisplayContext halftoneForm .
	originalColorMap := aDisplayContext colorMap.
	clr := (foreColor := aDisplayContext lastFontForegroundColor ifNil:[Color black asNontranslucentColor]) 
		pixelValueForDepth: 32.
	depth := aDisplayContext destForm depth.
	foreColorVal := clr bitAnd: 16rFFFFFF.
	foreColorAlpha := (clr bitAnd: 16rFF000000) >> 24.
	depth <= 8
		ifTrue:[
			aDisplayContext colorMap: (aDisplayContext cachedFontColormapFrom:32 to: depth)]
		ifFalse:[
			aDisplayContext colorMap: nil].
	startPointX := startPoint x truncated.
	startPointY := startPoint y.
	endPointX := endPoint x ceiling.
	endPointY := endPoint y.
	width := endPointX - startPointX.
	height := endPointY - startPointY.
	glyph := (Form extent: width@height depth: 32) fillWhite. "we could cache a big white glyph somewhere to save having to create this. Clipping will make only a part of it display"
	aDisplayContext sourceForm: glyph.
	aDisplayContext destOrigin: startPointX@startPointY.
	aDisplayContext width: width.
	aDisplayContext height: height.
	aDisplayContext 
		sourceOrigin: 0@0;
		halftoneForm: nil.
	(FreeTypeSettings current useSubPixelAntiAliasing and: [depth >= 8])
		ifTrue:[
			aDisplayContext 
				combinationRule: 41.
			aDisplayContext 
				copyBitsColor: foreColorVal 
				alpha: foreColorAlpha 
				gammaTable: FreeTypeSettings current gammaTable
				ungammaTable: FreeTypeSettings current gammaInverseTable]
		ifFalse:[
			glyph fillWithColor: foreColor.
			aDisplayContext combinationRule: (depth <= 8 ifTrue: [Form paint] ifFalse: [34]).
			aDisplayContext copyBits].		
	aDisplayContext 
		colorMap: originalColorMap;
		combinationRule: oldCombinationRule;
		halftoneForm: oldHalftoneForm.
	
	