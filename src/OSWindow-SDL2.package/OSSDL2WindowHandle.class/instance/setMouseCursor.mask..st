cursor
setMouseCursor: cursor mask: mask
	| cursorBits maskBits extent offset sdlCursor |
	cursorBits := self convertCursor: cursor.
	maskBits := self convertCursor: mask.
	extent := cursor extent.
	offset := cursor offset.
	sdlCursor := SDL2 createCursor: cursorBits mask: maskBits w: extent x h: extent y hotX: offset x negated hotY: offset y negated.
	sdlCursor ifNotNil: [ sdlCursor setCursor ].
	currentCursor ifNotNil: [ currentCursor freeCursor ].
	currentCursor := sdlCursor