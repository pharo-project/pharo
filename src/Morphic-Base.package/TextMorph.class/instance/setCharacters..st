accessing
setCharacters: chars
	"obtain a string value from the receiver"

	(self getCharacters = chars) ifFalse:
		[self newContents: chars]