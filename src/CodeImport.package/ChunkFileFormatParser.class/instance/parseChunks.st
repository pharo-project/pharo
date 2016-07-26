parsing
parseChunks
	[ readStream atEnd ] whileFalse: [ self parseNextDeclaration ].
	^ parsedDeclarations 