inflating
decompressBlock: llTable with: dTable
	"Process the compressed data in the block.
	llTable is the huffman table for literal/length codes
	and dTable is the huffman table for distance codes."
	| value extra length distance oldPos oldBits oldBitPos |
	<primitive: 'primitiveInflateDecompressBlock' module: 'ZipPlugin'>
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
			extra := (value bitShift: -16) - 1.
			length := value bitAnd: 16rFFFF.
			extra > 0 ifTrue:[length := length + (self nextBits: extra)].
			"Compute the distance value"
			value := self decodeValueFrom: dTable.
			extra := (value bitShift: -16).
			distance := value bitAnd: 16rFFFF.
			extra > 0 ifTrue:[distance := distance + (self nextBits: extra)].
			(readLimit + length >= collection size) ifTrue:[
				bitBuf := oldBits.
				bitPos := oldBitPos.
				sourcePos := oldPos.
				^self].
			collection 
					replaceFrom: readLimit+1 
					to: readLimit + length
					with: collection 
					startingAt: readLimit - distance + 1.
			readLimit := readLimit + length.
		].
	].