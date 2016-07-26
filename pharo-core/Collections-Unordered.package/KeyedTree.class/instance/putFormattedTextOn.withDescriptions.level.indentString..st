printing
putFormattedTextOn: aStream withDescriptions: aKeyedTree level: indentLevel indentString: aString
	"Place a print of the receiver and associated description on the given stream with the given indentation level."

	
	(self keys asSortedCollection: self sortBlock) do: [:k | | v |
		indentLevel timesRepeat: [aStream nextPutAll: aString].
		aStream nextPutAll: k printString.
		v := self at: k.
		(v isKindOf: self class)
			ifTrue: [aStream cr.
					v
						putFormattedTextOn: aStream
						withDescriptions: (aKeyedTree at: k ifAbsent: [self class new])
						level: indentLevel + 1
						indentString: aString]
			ifFalse: [aStream
						nextPutAll: ' : ';
						nextPutAll: v printString;
						tab; tab;
						nextPutAll: (aKeyedTree at: k ifAbsent: ['nondescript']) printString.
					aStream cr]]