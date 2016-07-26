*Graphics-Fonts
displayGlyph: aForm at: aPoint left: leftX right: rightX font: aFont
	"Display a glyph in a multi-lingual font. Do 2 pass rendering if necessary.
	This happens when #installStrikeFont:foregroundColor:backgroundColor: sets rule 37 (rgbMul).
	the desired effect is to do two bitblt calls. The first one is with rule 37 and special colormap.
	The second one is rule 34, with a colormap for applying the requested foreground color.
	This two together do component alpha blending, i.e. alpha blend red, green and blue separatedly.
	This is needed for arbitrary color over abitrary background text with subpixel AA."

	| prevRule secondPassMap |
	self sourceForm: aForm.
	destX := aPoint x.
	destY := aPoint y.
	sourceX := leftX.
	sourceY := 0.
	width := rightX - leftX.
	height := aFont height.
	combinationRule = 37 ifTrue:[
		"We need to do a second pass. The colormap set is for use in the second pass."
		secondPassMap := colorMap.
		colorMap := sourceForm depth = destForm depth
			ifFalse: [ self cachedFontColormapFrom: sourceForm depth to: destForm depth ].
		self copyBits.
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		colorMap := secondPassMap.
		self copyBits.
		combinationRule := prevRule.
	] ifFalse:[self copyBits].