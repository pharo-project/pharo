writing
writeMethodSource: definition
	self basicChunkContents: [:str | str nextPutAll: definition source ].