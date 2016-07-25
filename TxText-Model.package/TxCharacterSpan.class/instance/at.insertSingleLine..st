operation
at: aTxPosition insertSingleLine: aString
	"Insert a single line character string at the given position,
	answer a new position pointing to the end of inserted string"
	| insertionEnd |
	insertionEnd := self size - aTxPosition spanPosition.
	characters := String new: self size + aString size streamContents: [ :s|
		s
			next: aTxPosition spanPosition putAll: characters; 
			nextPutAll: aString;
			next: insertionEnd putAll: characters startingAt: aTxPosition spanPosition + 1 ].
	
	^ self positionAt: aTxPosition spanPosition + aString size.