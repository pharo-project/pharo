*Graphics-Fonts
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta

	destY := aPoint y.
	destX := aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX := 100].
	width ifNil: [width := 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: kernDelta.
	^ destX@destY.
