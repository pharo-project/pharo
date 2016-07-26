instance creation
forDoit: aString
	^ (self subclassForDoit: aString) ifNotNil: [:c | c new source: aString]