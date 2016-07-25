displaying
characters: anInterval in: sourceString displayAt: aPoint clippedBy: clippingRectangle rule: ruleInteger fillColor: aForm kernDelta: kernDelta on: aBitBlt 
	"Simple, slow, primitive method for displaying a line of characters.
	No wrap-around is provided."
	| destPoint |
	destPoint := aPoint.
	anInterval do: 
		[ :i | | rightX sourceRect leftX ascii | 
		self flag: #yoDisplay.
		"if the char is not supported, fall back to the specified fontset."
		ascii := (sourceString at: i) charCode.
		(ascii < minAscii or: [ ascii > maxAscii ]) ifTrue: [ ascii := maxAscii ].
		leftX := xTable at: ascii + 1.
		rightX := xTable at: ascii + 2.
		sourceRect := leftX @ 0 extent: (rightX - leftX) @ self height.
		aBitBlt 
			copyFrom: sourceRect
			in: glyphs
			to: destPoint.
		destPoint := destPoint + ((rightX - leftX + kernDelta) @ 0)
		"destPoint printString displayAt: 0@(i*20)" ].
	^ destPoint