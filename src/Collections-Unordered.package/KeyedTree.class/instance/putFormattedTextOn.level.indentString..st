printing
putFormattedTextOn: aStream level: indentLevel indentString: aString
	"Place a description of the receiver on the given stream with the given indentation level."

	
	(self keys asSortedCollection: self sortBlock) do: [:k | | v |
		indentLevel timesRepeat: [aStream nextPutAll: aString].
		aStream nextPutAll: k printString.
		v := self at: k.
		(v isKindOf: self class)
			ifTrue: [aStream cr.
					v putFormattedTextOn: aStream level: indentLevel + 1 indentString: aString]
			ifFalse: [aStream
						nextPutAll: ' : ';
						nextPutAll: v printString.
					aStream cr]]