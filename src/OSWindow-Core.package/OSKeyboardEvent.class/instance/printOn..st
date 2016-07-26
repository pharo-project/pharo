printing
printOn: stream
	super printOn: stream.
	stream space; nextPutAll: 'scanCode: '; print: scanCode.
	stream space; nextPutAll: 'symbol '; print: symbol.
	stream space; nextPutAll: 'character '; print: character.
	stream space; nextPutAll: 'modifiers: '; print: modifiers.
	