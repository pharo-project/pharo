operations
insert: aString 
	"
	Insert a possibly multi-line string at given position of receiver.
	Move position right after the last character of inserted string.
	The inserted portion of will inherit same attributes as current position attributes.
	"
	position := self span at: position insert: aString.
