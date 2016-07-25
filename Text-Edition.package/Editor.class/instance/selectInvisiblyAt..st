new selection
selectInvisiblyAt: characterIndex 
	"Place the caret before the character at characterIndex.
	 Be sure it is in view but vithout any further action."
	
	self selectInvisiblyFrom: characterIndex to:  characterIndex - 1
