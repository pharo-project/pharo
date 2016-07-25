displaying
displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY
	| glyph  depth foreColorVal foreColorAlpha originalColorMap clr subPixelPosition widthAndKernedWidth char nextChar floatDestX  destX destY offset gammaTable gammaInverseTable useRule41 |
	
	useRule41 := FreeTypeSettings current useSubPixelAntiAliasing and: [aBitBlt destForm depth >= 8].
	depth := aBitBlt destForm depth.
	originalColorMap := aBitBlt colorMap.
	clr := (aBitBlt lastFontForegroundColor ifNil:[Color black asNontranslucentColor]) 
		pixelValueForDepth: 32.
	useRule41 
		ifTrue:[
			foreColorVal := clr bitAnd: 16rFFFFFF.
			foreColorAlpha := (clr bitAnd: 16rFF000000) >> 24.
			gammaTable := FreeTypeSettings current gammaTable.
			gammaInverseTable := FreeTypeSettings current gammaInverseTable.]
		ifFalse:[
			foreColorVal := clr].
	depth <= 8
		ifTrue:[
			aBitBlt colorMap: (aBitBlt cachedFontColormapFrom:32 to: depth)]
		ifFalse:[
			aBitBlt colorMap: nil].
	destX := aPoint x.
	destY := baselineY.
	floatDestX := aPoint x.
	widthAndKernedWidth := Array new: 2.
 	startIndex to: stopIndex do: [:i | 
		subPixelPosition := ((floatDestX \\ 1) roundTo: "1/64" 0.015625) * 64.
		subPixelPosition = 64 
			ifTrue:[
				subPixelPosition := 0.
				destX := destX + 1].
		char := aString at: i.
		glyph := self 
			glyphOf: char 
			destDepth: depth 
			colorValue: foreColorVal 
			subpixelPosition: subPixelPosition.
		aBitBlt sourceForm: glyph.
		offset := glyph offset.
		aBitBlt destX: destX + offset x.
		aBitBlt destY: destY + offset y.
		aBitBlt width: glyph width.
		aBitBlt height: glyph height.
		useRule41
			ifTrue:[
				aBitBlt 
					copyBitsColor: foreColorVal 
					alpha: foreColorAlpha 
					gammaTable: gammaTable
					ungammaTable: gammaInverseTable]
			ifFalse:[
				aBitBlt copyBits].
		nextChar := (i + 1 <= stopIndex) 
				ifTrue:[aString at: i + 1]
				ifFalse:[nil].
		self 
			widthAndKernedWidthOfLeft: char 
			right: nextChar
			into: widthAndKernedWidth.
		floatDestX := floatDestX + (widthAndKernedWidth at: 2) + kernDelta.
		destX := floatDestX ].
	aBitBlt colorMap: originalColorMap.
	^ destX @ destY 
