filein/out
readAttributesFrom: aBinaryStream
	| offsetX offsetY |
	depth := aBinaryStream next.
	(self depth isPowerOfTwo and: [self depth between: 1 and: 32])
		ifFalse: [self error: 'invalid depth; bad Form file?'].
	width := aBinaryStream nextWord.
	height := aBinaryStream nextWord.
	offsetX  := aBinaryStream nextWord.
	offsetY := aBinaryStream nextWord.
	offsetX > 32767 ifTrue: [offsetX := offsetX - 65536].
	offsetY > 32767 ifTrue: [offsetY := offsetY - 65536].
	offset := Point x: offsetX y: offsetY.
	
