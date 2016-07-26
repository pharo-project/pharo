operation
insertCharacters: chars at: position attributes: insertAttributes
	"Insert a single line character string at the given position,
	answer a new position pointing to the end of inserted string"

	| spanPos newSpan |

	spanPos := position spanPosition.
	spanPos = 0 ifTrue: [ 
		prev isCharacterSpan ifTrue: [ 
			^ prev insertCharacters: chars at: prev endPosition attributes: insertAttributes ].
		newSpan := (text newCharacterSpan: chars) replaceAttributes: insertAttributes.
		self insertPrev: newSpan.
		^ position	
	].
	
	next isCharacterSpan ifTrue: [ 
		^ next insertCharacters: chars at: next startPosition attributes: insertAttributes ].
	newSpan := (text newCharacterSpan: chars) replaceAttributes: insertAttributes.
	self insertNext: newSpan.
	^ next endPosition