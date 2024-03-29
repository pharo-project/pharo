Extension { #name : 'BitBlt' }

{ #category : '*Graphics-Fonts' }
BitBlt >> basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale [

	| xTable |

	destY := aPoint y.
	destX := aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX := 100].
	width ifNil: [width := 100].
	xTable := font xTable.
	scale = 1 ifFalse: [
		xTable := xTable collect: [ :x | x * scale ] ].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: xTable
			kern: kernDelta * scale.
	^ destX@destY
]

{ #category : '*Graphics-Fonts' }
BitBlt >> cachedFontColormapFrom: sourceDepth to: destDepth [
	"Modified from computeColormapFrom:to:."
	| srcIndex map |
	CachedFontColorMaps class == Array ifFalse: [ CachedFontColorMaps := (1 to: 9) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: [ srcIndex := 9 ].
	(map := (CachedFontColorMaps at: srcIndex) at: destDepth) ~~ nil ifTrue: [ ^ map ].
	map := (Color
		cachedColormapFrom: sourceDepth
		to: destDepth) copy.
	(CachedFontColorMaps at: srcIndex)
		at: destDepth
		put: map.
	^ map
]

{ #category : '*Graphics-Fonts' }
BitBlt >> displayString: aString from: startIndex to: stopIndex at: aPoint kern: kernDelta font: aFont [
	"Double dispatch into the font. This method is present so that other-than-bitblt entities can be used by CharacterScanner and friends to display text."
	^ aFont displayString: aString on: self from: startIndex to: stopIndex at: aPoint kern: kernDelta
]

{ #category : '*Graphics-Fonts' }
BitBlt >> displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale [
	"If required, do a second pass with new rule and colorMap.
	This happens when #installStrikeFont:foregroundColor:backgroundColor: sets rule 37 (rgbMul).
	the desired effect is to do two bitblt calls. The first one is with rule 37 and special colormap.
	The second one is rule 34, with a colormap for applying the requested foreground color.
	This two together do component alpha blending, i.e. alpha blend red, green and blue separatedly.
	This is needed for arbitrary color over abitrary background text with subpixel AA."

	| answer prevRule secondPassMap |
	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale ].

	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap := colorMap.
	colorMap := sourceForm depth ~= destForm depth
		ifTrue: [ self cachedFontColormapFrom: sourceForm depth to: destForm depth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale.
		combinationRule := prevRule ].
	^answer
]

{ #category : '*Graphics-Fonts' }
BitBlt >> installFont: aFont foregroundColor: foregroundColor backgroundColor: backgroundColor [
	"Double dispatch into the font. This method is present so that other-than-bitblt entities can be used by CharacterScanner and friends to display text."
	^aFont installOn: self foregroundColor: foregroundColor backgroundColor: backgroundColor
]

{ #category : '*Graphics-Fonts' }
BitBlt >> installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [
	| lastSourceDepth targetColor |
	sourceForm ifNotNil:[lastSourceDepth := sourceForm depth].
	sourceForm := aStrikeFont glyphs.
	scale = 1 ifFalse: [
		sourceForm := sourceForm scaledToSize: sourceForm extent * scale ].

	"Ignore any halftone pattern since we use a color map approach here"
	halftoneForm := nil.
	sourceY := 0.
	height := aStrikeFont height * scale.

	sourceForm depth = 1 ifTrue: [
		self combinationRule: Form paint.
		(colorMap isNotNil and:[lastSourceDepth = sourceForm depth]) ifFalse: [
			"Set up color map for a different source depth (color font)"
			"Uses caching for reasonable efficiency"
			colorMap := self cachedFontColormapFrom: sourceForm depth to: destForm depth.
			colorMap at: 1 put: (destForm pixelValueFor: backgroundColor)].
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor).
	]
	ifFalse: [
			destForm depth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"RGBMul"
				colorMap := (destForm depth = 32 or: [ (foregroundColor = Color black) not ]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:kern:"
					"Note: In 32bpp we always need the second pass, as the source could have transparent pixels, and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceForm depth to: destForm depth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor := foregroundColor = Color black ifFalse: [ foregroundColor ].
				colorMap := self colorConvertingMap: targetColor from: sourceForm depth to: destForm depth keepSubPixelAA: true]
		]
]
