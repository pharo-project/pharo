marker parsing
parseHuffmanTable
	| length markerStart index bits count huffVal isACTable hTable |
	markerStart := self position.
	length := self nextWord.
	[ self position - markerStart >= length ] whileFalse: 
		[ index := self next.
		isACTable := (index bitAnd: 16) ~= 0.
		index := (index bitAnd: 15) + 1.
		index > HuffmanTableSize ifTrue: 
			[ self error: 'image has more than ' , HuffmanTableSize printString , ' quantization tables' ].
		bits := self next: 16.
		count := bits sum.
		(count > 256 or: [ count > (length - (self position - markerStart)) ]) ifTrue: [ self error: 'Huffman Table count is incorrect' ].
		huffVal := self next: count.
		hTable := stream 
			buildLookupTable: huffVal
			counts: bits.
		isACTable 
			ifTrue: 
				[ self hACTable 
					at: index
					put: hTable ]
			ifFalse: 
				[ self hDCTable 
					at: index
					put: hTable ] ]