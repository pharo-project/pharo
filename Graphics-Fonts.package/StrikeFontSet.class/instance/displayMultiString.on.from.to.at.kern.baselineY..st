displaying
displayMultiString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY

	| destPoint leftX rightX glyphInfo g destY |
	destPoint := aPoint.
	glyphInfo := Array new: 5.
	startIndex to: stopIndex do: [:charIndex |
		self glyphInfoOf: (aString at: charIndex) into: glyphInfo.
		g := glyphInfo at:1.
		leftX := glyphInfo at:2.
		rightX := glyphInfo at:3.
		((glyphInfo at:5) ~= aBitBlt lastFont) ifTrue: [
			(glyphInfo at:5) installOn: aBitBlt.
		].
		aBitBlt sourceForm: g.
		destY := baselineY - (glyphInfo at:4).
		aBitBlt destX: (destPoint x) destY: destY width: (rightX - leftX) height: (self height).
		aBitBlt sourceOrigin: leftX @ 0.
		aBitBlt copyBits.
		destPoint := destPoint x + (rightX - leftX + kernDelta) @ destPoint y.
	].
	^ destPoint.