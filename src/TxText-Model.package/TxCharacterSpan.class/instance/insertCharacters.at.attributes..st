operation
insertCharacters: chars at: position attributes: insertAttributes
	"Insert a single line character string at the given position,
	answer a new position pointing to the end of inserted string"

	| spanPos insertionEnd |

	spanPos := position spanPosition.
	
	style = insertAttributes ifFalse: [
		"going to create a separate character span, since attributes are incompatible"
		| newSpan |
		newSpan := (text newCharacterSpan: chars) replaceAttributes: insertAttributes.
		
		spanPos = 0 ifTrue: [
			self insertPrev: newSpan.
			^ self startPosition
			].
		
		spanPos < self size ifTrue: [ self splitAt: spanPos ].
			
		self insertNext: newSpan.
		^ newSpan endPosition.
	].
		
	"attributes are same, just insert characters"
 	insertionEnd := self size - spanPos.
	characters := String new: self size + chars size streamContents: [ :s|
		s
			next: spanPos putAll: characters; 
			nextPutAll: chars;
			next: insertionEnd putAll: characters startingAt: spanPos + 1 ].
	
	^ self positionAt: spanPos + chars size.