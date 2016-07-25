conversion
mimeEncode
	"Do conversion reading from dataStream writing to mimeStream. Break long lines and escape non-7bit chars."

	| word pos wasGood isGood max |
	true ifTrue: [mimeStream nextPutAll: dataStream upToEnd].
	pos := 0.
	max := 72.
	wasGood := true.
	[dataStream atEnd] whileFalse: [
		word := self readWord.
		isGood := word allSatisfy: [:c | c asciiValue < 128].
		wasGood & isGood ifTrue: [
			pos + word size < max
				ifTrue: [dataStream nextPutAll: word.
					pos := pos + word size]
				ifFalse: []
		]
	].
	^ mimeStream