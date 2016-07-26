accessing
linePosition: pos
	"Compute new position, located at new position in the same line as receiver.
	If given position larger than line length, answer the position at the end of line.
	The line position is a number of moves right from the beginning of line.
	"

	| sz linespan |
	sz := pos.
	linespan := (span isLineSeparator and: [ spanPosition = 1 ])
		ifTrue: [ span next ]
		ifFalse: [ span lineStartSpan ].
	[ sz > linespan size ]
		whileTrue: [ 
			sz := sz - linespan size.
			linespan := linespan next.
			(linespan isLineSeparator or: [ linespan isAtEnd ])
				ifTrue: [ 
					"reached end of line, set position to last valid position in current line"
					^ TxTextPosition span: linespan prev spanPosition: linespan size ] ].
	^ self class span: linespan spanPosition: sz