operation
insertCharacters: chars at: position attributes: insertAttributes
	
	"Insert characters (no line separators allowed!!!), with given attributes at given position in receiver.
	Answer the end position of inserted characters"
	
	"Since we cannot insert anything into end span, and its only valid position is zero,
	we either delegate to the previous character span, or create a new one, if none"
	
	| span |
	prev isCharacterSpan ifTrue: [
		^ prev insertCharacters: chars at: prev endPosition attributes: insertAttributes 
	].

	span := (text newCharacterSpan: chars) replaceAttributes: insertAttributes.
	self insertPrev: span.
	^ position
	