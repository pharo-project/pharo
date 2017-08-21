character printing
nextPut: aCharacter
	"When a character is put, force the style changes"

	self changeStyle.
	self outStream nextPut: aCharacter.
	self flush