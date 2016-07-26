typing/selecting keys
lf: aKeyboardEvent 
	"Append a line feed character to the stream of characters."

	self addString: Character lf asString.
	^false