private-encoding
readWord

	| strm |
	strm := (String new: 20) writeStream.
	dataStream skipSeparators.
	[dataStream atEnd] whileFalse: 
		[ | c |
		c := dataStream next.
		strm nextPut: c.
		c isSeparator ifTrue: [^ strm contents]].
	^ strm contents