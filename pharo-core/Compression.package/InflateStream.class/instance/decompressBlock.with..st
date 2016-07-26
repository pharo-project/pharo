inflating
decompressBlock: llTable with: dTable
	"Process the compressed data in the block.
	llTable is the huffman table for literal/length codes
	and dTable is the huffman table for distance codes."
	| value extra length distance oldPos oldBits oldBitPos |
	[readLimit < collection size and:[sourcePos <= sourceLimit]] whileTrue:[
		"Back up stuff if we're running out of space"
		oldBits := bitBuf.
		oldBitPos := bitPos.
		oldPos := sourcePos.
		value := self decodeValueFrom: llTable.
		value < 256 ifTrue:[ "A literal"
			collection byteAt: (readLimit := readLimit + 1) put: value.
		] ifFalse:["length/distance or end of block"
			value = 256 ifTrue:["End of block"
				state := state bitAnd: StateNoMoreData.
				^self].
			"Compute the actual length value (including possible extra bits)"
			extra := #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0) at: value - 256.
			length := #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258) at: value - 256.
			extra > 0 ifTrue:[length := length + (self nextBits: extra)].
			"Compute the distance value"
			value := self decodeValueFrom: dTable.
			extra := #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13) at: value+1.
			distance := #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
						1025 1537 2049 3073 4097 6145 8193 12289 16385 24577) at: value+1.
			extra > 0 ifTrue:[distance := distance + (self nextBits: extra)].
			(readLimit + length >= collection size) ifTrue:[
				bitBuf := oldBits.
				bitPos := oldBitPos.
				sourcePos := oldPos.
				^self].
			collection 
					replaceFrom: readLimit+1 
					to: readLimit + length + 1 
					with: collection 
					startingAt: readLimit - distance + 1.
			readLimit := readLimit + length.
		].
	].