*Graphics-Fonts
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta
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
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap := colorMap.
	colorMap := sourceForm depth ~= destForm depth
		ifTrue: [ self cachedFontColormapFrom: sourceForm depth to: destForm depth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta.
		combinationRule := prevRule ].
	^answer