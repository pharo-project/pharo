marker parsing
parseStartOfFile
	| length markerStart value n |
	markerStart := self position.
	length := self nextWord.
	dataPrecision := self next.
	dataPrecision = 8 ifFalse: 
		[ self error: 'cannot handle ' , dataPrecision printString , '-bit components' ].
	height := self nextWord.
	width := self nextWord.
	n := self next.
	height = 0 | (width = 0) | (n = 0) ifTrue: [ self error: 'empty image' ].
	length - (self position - markerStart) ~= (n * 3) ifTrue: [ self error: 'component length is incorrect' ].
	components := Array new: n.
	1 
		to: components size
		do: 
			[ :i | 
			components 
				at: i
				put: (JPEGColorComponent new
						id: self next;
						widthInBlocks: ((value := self next) >> 4 bitAnd: 15);
						heightInBlocks: (value bitAnd: 15);
						qTableIndex: self next + 1)
			"heightInBlocks: (((value := self next) >> 4) bitAnd: 16r0F);
					widthInBlocks: (value bitAnd: 16r0F);" ]