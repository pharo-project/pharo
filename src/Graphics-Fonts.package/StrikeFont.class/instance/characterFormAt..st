character shapes
characterFormAt: character 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii := character charCode.
	(ascii 
		between: minAscii
		and: maxAscii) ifFalse: [ ascii := maxAscii + 1 ].
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	leftX < 0 ifTrue: [ ^ glyphs copy: (0 @ 0 corner: 0 @ self height) ].
	^ glyphs copy: (leftX @ 0 corner: rightX @ self height)