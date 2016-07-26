typing/selecting keys
crlf: aKeyboardEvent
	"Append a line feed character to the stream of characters."

	self addString: String crlf.
	^false