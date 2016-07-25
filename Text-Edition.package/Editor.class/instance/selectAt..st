new selection
selectAt: characterIndex 
	"Place the caret before the character at characterIndex.
	 Be sure it is in view."

	self selectFrom: characterIndex to: characterIndex - 1