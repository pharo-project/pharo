"
This class adds the following optimizations to the basic Inflate decompression:

a) Bit reversed access
If we want to fetch the bits efficiently then we have them in the wrong bit order (e.g., when we should fetch 2r100 we would get 2r001). But since the huffman tree lookup determines the efficiency of the decompression, reversing the bits before traversal is expensive. Therefore the entries in each table are stored in REVERSE BIT ORDER. This is achieved by a reverse increment of the current table index in the huffman table construction phase (see method increment:bits:). According to my measures this speeds up the implementation by about 30-40%.

b) Inplace storage of code meanings and extra bits
Rather than looking up the meaning for each code during decompression of blocks we store the appropriate values directly in the huffman tables, using a pre-defined mapping. Even though this does not make a big difference in speed, it cleans up the code and allows easier translation into primitive code (which is clearly one goal of this implementation).

c) Precomputed huffman tables for fixed blocks
So we don't have to compute the huffman tables from scratch. The precomputed tables are not in our superclass to avoid double storage (and my superclass is more intended for documentation anyways).
"
Class {
	#name : #FastInflateStream,
	#superclass : #InflateStream,
	#classVars : [
		'DistanceMap',
		'FixedDistTable',
		'FixedLitTable',
		'LiteralLengthMap'
	],
	#category : #Compression-Streams
}

{ #category : #initialization }
FastInflateStream class >> initialize [
	"FastInflateStream initialize"
	| low high |

	"Init literal/length map"
	low := #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 ).
	high := #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0).
	LiteralLengthMap := WordArray new: 256 + 32.
	1 to: 257 do:[:i| LiteralLengthMap at: i put: i-1].
	1 to: 29 do:[:i| LiteralLengthMap at: 257+i put: (low at:i) + ( (high at: i) + 1 << 16)].

	"Init distance map"
	high := #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	low := #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
			1025 1537 2049 3073 4097 6145 8193 12289 16385 24577).
	DistanceMap := WordArray new: 32.
	1 to: 30 do:[:i| DistanceMap at: i put: (low at: i) + ( (high at: i) << 16)].

	"Init fixed block huffman tables"
	FixedLitTable := self basicNew
				huffmanTableFrom: FixedLitCodes
				mappedBy: LiteralLengthMap.
	FixedDistTable := self basicNew
				huffmanTableFrom: FixedDistCodes
				mappedBy: DistanceMap.
]

{ #category : #inflating }
FastInflateStream >> decompressBlock: llTable with: dTable [
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
]

{ #category : #'huffman trees' }
FastInflateStream >> distanceMap [
	^DistanceMap
]

{ #category : #'huffman trees' }
FastInflateStream >> increment: value bits: nBits [
	"Increment value in reverse bit order, e.g. 
	for a 3 bit value count as follows:
		000 / 100 / 010 / 110
		001 / 101 / 011 / 111
	See the class comment why we need this."
	| result bit |
	result := value.
	"Test the lowest bit first"
	bit := 1 << (nBits - 1).
	"If the currently tested bit is set then we need to
	turn this bit off and test the next bit right to it"
	[(result bitAnd: bit) = 0] whileFalse:[ 
		"Turn off current bit"
		result := result bitXor: bit.
		"And continue testing the next bit"
		bit := bit bitShift: -1].
	"Turn on the right-most bit that we haven't touched in the loop above"
	^result bitXor: bit
]

{ #category : #'huffman trees' }
FastInflateStream >> literalLengthMap [
	^LiteralLengthMap
]

{ #category : #'bit access' }
FastInflateStream >> nextSingleBits: n [
	"Fetch the bits all at once"
	^self nextBits: n.
]

{ #category : #inflating }
FastInflateStream >> processFixedBlock [
	litTable := FixedLitTable.
	distTable := FixedDistTable.
	state := state bitOr: BlockProceedBit.
	self proceedFixedBlock.
]
