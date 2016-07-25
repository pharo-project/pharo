character shapes
characterFormAt: character 

	| encoding ascii xTable leftX rightX |
	encoding := character leadingChar + 1.
	ascii := character charCode.
	(ascii < (fontArray at: encoding) minAscii or: [ascii > (fontArray at: encoding) maxAscii])
		ifTrue: [ascii := (fontArray at: encoding) maxAscii].
	xTable := (fontArray at: encoding) xTable.
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	^ (fontArray at: encoding) glyphs copy: (leftX @ 0 corner: rightX @ self height).
