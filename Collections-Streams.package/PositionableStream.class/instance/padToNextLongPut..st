positioning
padToNextLongPut: char 
	"Make position be on long word boundary, writing the padding 
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]