parsing
parseNextDeclaration
	| isMetadata nextChunk |
	readStream skipSeparators.
	readStream isNextStyleChunk
		ifTrue:
			[ 
			self addDeclaration: (self styleChunkClass contents: readStream readUpToEndOfStyleChunk).
			^ self ].
	isMetadata := readStream isNextChunkMetaData.
	nextChunk := readStream next.
	isMetadata
		ifFalse: [ self addDeclaration: (self doItChunkClass contents: nextChunk) ]
		ifTrue:
			[ 
			| substrings |
			substrings := nextChunk parseLiterals.
			(substrings includes: self methodsForSelector asString)
				ifTrue: [ ^ self parseMethodDeclarations: substrings ].
			(substrings includes: self commentStampSelector asString)
				ifTrue: [ ^ self parseCommentDeclaration: substrings ].
			(substrings includes: self reorganizeSelector asString)
				ifTrue: [ ^ self parseClassOrganization: substrings ] ]