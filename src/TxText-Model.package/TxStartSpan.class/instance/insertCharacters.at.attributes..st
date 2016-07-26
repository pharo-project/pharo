operation
insertCharacters: chars at: position attributes: insertAttributes
	
	"Insert characters (no line separators allowed!!!), with given attributes at given position in receiver.
	Answer the end position of inserted characters"
	
	"Since we cannot insert anything into start span, and our only valid position is zero,
	we either delegate to next character span, or create a new one, if none"
	
	| span |
	next isCharacterSpan ifTrue: [
		^ next insertCharacters: chars at: next startPosition attributes: insertAttributes 
	].

	span := (text newCharacterSpan: chars) replaceAttributes: insertAttributes.
	self insertNext: span.
	^ next endPosition
	