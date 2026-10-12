Extension { #name : 'BitBlt' }

{ #category : '*Graphics-Fonts' }
BitBlt >> basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta scale: scale [

	| xTable |
	self destY: aPoint y.
	self destX: aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	self sourceX ifNil: [ self sourceX: 100 ].
	self width ifNil: [ self width: 100 ].
	xTable := font xTable.
	scale = 1 ifFalse: [ xTable := xTable collect: [ :x | x * scale ] ].

	self
		primDisplayString: aString
		from: startIndex
		to: stopIndex
		map: font characterToGlyphMap
		xTable: xTable
		kern: kernDelta * scale.
	^ self destX @ self destY
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
	self combinationRule = 37 ifFalse: [
			^ self
				  basicDisplayString: aString
				  from: startIndex
				  to: stopIndex
				  at: aPoint
				  strikeFont: font
				  kern: kernDelta
				  scale: scale ]. "rgbMul"

	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap := self colorMap.
	self colorMap: (self sourceForm depth ~= self destForm depth ifTrue: [
			 self cachedFontColormapFrom: self sourceForm depth to: self destForm depth ]).
	answer := self
		          basicDisplayString: aString
		          from: startIndex
		          to: stopIndex
		          at: aPoint
		          strikeFont: font
		          kern: kernDelta
		          scale: scale.
	self colorMap: secondPassMap.
	secondPassMap ifNotNil: [
			prevRule := self combinationRule.
			self combinationRule: 20. "rgbAdd"
			self
				basicDisplayString: aString
				from: startIndex
				to: stopIndex
				at: aPoint
				strikeFont: font
				kern: kernDelta
				scale: scale.
			self combinationRule: prevRule ].
	^ answer
]

{ #category : '*Graphics-Fonts' }
BitBlt >> installFont: aFont foregroundColor: foregroundColor backgroundColor: backgroundColor [
	"Double dispatch into the font. This method is present so that other-than-bitblt entities can be used by CharacterScanner and friends to display text."
	^aFont installOn: self foregroundColor: foregroundColor backgroundColor: backgroundColor
]

{ #category : '*Graphics-Fonts' }
BitBlt >> installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [

	| lastSourceDepth targetColor |
	self sourceForm ifNotNil: [ lastSourceDepth := self sourceForm depth ].
	self sourceForm: aStrikeFont glyphs.
	scale = 1 ifFalse: [ self sourceForm: (self sourceForm scaledToSize: self sourceForm extent * scale) ].

	"Ignore any halftone pattern since we use a color map approach here"
	self halftoneForm: nil.
	self sourceY: 0.
	self height: aStrikeFont height * scale.

	self sourceForm depth = 1
		ifTrue: [
				self combinationRule: Form paint.
				(self colorMap isNotNil and: [ lastSourceDepth = self sourceForm depth ]) ifFalse: [ "Set up color map for a different source depth (color font)""Uses caching for reasonable efficiency"
						self colorMap: (self cachedFontColormapFrom: self sourceForm depth to: self destForm depth).
						self colorMap at: 1 put: (self destForm pixelValueFor: backgroundColor) ].
				self colorMap at: 2 put: (self destForm pixelValueFor: foregroundColor) ]
		ifFalse: [
				self destForm depth > 8
					ifTrue: [ "rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
							self combinationRule: 37. "RGBMul"
							self colorMap:
								((self destForm depth = 32 or: [ (foregroundColor = Color black) not ]) ifTrue: [ "rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!)""This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:kern:""Note: In 32bpp we always need the second pass, as the source could have transparent pixels, and we need to add to the alpha channel"
										 self
											 colorConvertingMap: foregroundColor
											 from: self sourceForm depth
											 to: self destForm depth
											 keepSubPixelAA: true ]) ]
					ifFalse: [
							self combinationRule: 25. "Paint"
							targetColor := foregroundColor = Color black ifFalse: [ foregroundColor ].
							self colorMap: (self
									 colorConvertingMap: targetColor
									 from: self sourceForm depth
									 to: self destForm depth
									 keepSubPixelAA: true) ] ]
]
