operation
insertCharacters: chars at: position attributes: insertAttributes

	"Insert characters (no line separators allowed!!!), with given attributes at given position into receiver.
	Answer the end position of inserted characters"

	
	self subclassResponsibility 