"
This class implements the Inflate decompression algorithm as defined by RFC1951 and used in PKZip, GZip and ZLib (and many, many more). It is a variant of the LZ77 compression algorithm described in

[LZ77] Ziv J., Lempel A., ""A Universal Algorithm for Sequential Data Compression"", IEEE Transactions on Information Theory"", Vol. 23, No. 3, pp. 337-343.

[RFC1951] Deutsch. P, ""DEFLATE Compressed Data Format Specification version 1.3""

For more information see the above mentioned RFC 1951 which can for instance be found at

	http://www.leo.org/pub/comp/doc/standards/rfc/index.html

Huffman Tree Implementation Notes:
===========================================
The huffman tree used for decoding literal, distance and length codes in the inflate algorithm has been encoded in a single Array. The tree is made up of subsequent tables storing all entries at the current bit depth. Each entry in the table (e.g., a 32bit Integer value) is either a leaf or a non-leaf node. Leaf nodes store the immediate value in its low 16 bits whereas non-leaf nodes store the offset of the subtable in its low 16bits. The high 8 bits of non-leaf nodes contain the number of additional bits needed for the sub table (the high 8 bits of leaf-nodes are always zero). The first entry in each table is always a non-leaf node indicating how many bits we need to fetch initially. We can thus travel down the tree as follows (written in sort-of-pseudocode the actual implementation can be seen in InflateStream>>decodeValueFrom:):

	table := initialTable.
	bitsNeeded := high 8 bits of (table at: 1).		""Determine initial bits""
	table := initialTable + (low 16 bits of (table at: 1)). ""Determine start of first real table""
	[bits := fetch next bitsNeeded bits.			""Grab the bits""
	value := table at: bits.						""Lookup the value""
	value has high 8 bit set] whileTrue:[		""Check if it's leaf""
		table := initialTable + (low 16 bits of value).	""No - compute new sub table start""
		bitsNeeded := high 8 bit of value].		""Compute additional number of bits needed""
	^value

"
Class {
	#name : #InflateStream,
	#superclass : #ReadStream,
	#instVars : [
		'state',
		'bitBuf',
		'bitPos',
		'source',
		'sourcePos',
		'sourceLimit',
		'litTable',
		'distTable',
		'sourceStream',
		'crc'
	],
	#classVars : [
		'BlockProceedBit',
		'BlockTypes',
		'FixedDistCodes',
		'FixedLitCodes',
		'MaxBits',
		'StateNewBlock',
		'StateNoMoreData'
	],
	#category : #'Compression-Streams'
}

{ #category : #initialization }
InflateStream class >> initialize [
	"InflateStream initialize"
	MaxBits := 16.
	StateNewBlock := 0.
	StateNoMoreData := 1.
	BlockProceedBit := 8.
	BlockTypes := #(	processStoredBlock	"New block in stored format"
					processFixedBlock	"New block with fixed huffman tables"
					processDynamicBlock	"New block with dynamic huffman tables"
					errorBadBlock		"Bad block format"
					proceedStoredBlock	"Continue block in stored format"
					proceedFixedBlock	"Continue block in fixed format"
					proceedDynamicBlock	"Continue block in dynamic format"
					errorBadBlock		"Bad block format").
	"Initialize fixed block values"
	FixedLitCodes := 	((1 to: 144) collect:[:i| 8]),
					((145 to: 256) collect:[:i| 9]),
					((257 to: 280) collect:[:i| 7]),
					((281 to: 288) collect:[:i| 8]).
	FixedDistCodes := ((1 to: 32) collect:[:i| 5]).
]

{ #category : #testing }
InflateStream >> atEnd [
	"Note: It is possible that we have a few bits left,
	representing just the EOB marker. To check for
	this we must force decompression of the next
	block if at end of data."
	super atEnd ifFalse:[^false]. "Primitive test"
	(position >= readLimit and:[state = StateNoMoreData]) ifTrue:[^true].
	"Force decompression, by calling #next. Since #moveContentsToFront
	will never move data to the beginning of the buffer it is safe to
	skip back the read position afterwards"
	self next ifNil: [^true].
	position := position - 1.
	^false
]

{ #category : #'bit access' }
InflateStream >> bitPosition [
	"Return the current bit position of the source"
	sourceStream == nil
		ifTrue:[^sourcePos * 8 + bitPos]
		ifFalse:[^sourceStream position + sourcePos * 8 + bitPos]
]

{ #category : #'open/close' }
InflateStream >> close [
	sourceStream ifNotNil:[sourceStream close].
]

{ #category : #'huffman trees' }
InflateStream >> computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits [
	"Assign numerical values to all codes.
	Note: The values are stored according to the bit length"
	| offsets values baseOffset codeLength |
	offsets := Array new: maxBits.
	offsets atAllPut: 0.
	baseOffset := 1.
	minBits to: maxBits do:[:bits|
		offsets at: bits put: baseOffset.
		baseOffset := baseOffset + (counts at: bits+1)].
	values := WordArray new: aCollection size.
	1 to: aCollection size do:[:i|
		codeLength := aCollection at: i.
		codeLength > 0 ifTrue:[
			baseOffset := offsets at: codeLength.
			values at: baseOffset put: i-1.
			offsets at: codeLength put: baseOffset + 1]].
	^values
]

{ #category : #accessing }
InflateStream >> contents [

	^ self upToEnd
]

{ #category : #crc }
InflateStream >> crcError: aString [
	^CRCError signal: aString
]

{ #category : #'huffman trees' }
InflateStream >> createHuffmanTables: values counts: counts from: minBits to: maxBits [
	"Create the actual tables"
	| table tableStart tableSize tableEnd 
	valueIndex tableStack numValues deltaBits maxEntries
	lastTable lastTableStart tableIndex lastTableIndex |

	table := WordArray new: ((4 bitShift: minBits) max: 16).

	"Create the first entry - this is a dummy.
	It gives us information about how many bits to fetch initially."
	table at: 1 put: (minBits bitShift: 24) + 2. "First actual table starts at index 2"

	"Create the first table from scratch."
	tableStart := 2. "See above"
	tableSize := 1 bitShift: minBits.
	tableEnd := tableStart + tableSize.
	"Store the terminal symbols"
	valueIndex := (counts at: minBits+1).
	tableIndex := 0.
	1 to: valueIndex do:[:i|
		table at: tableStart + tableIndex put: (values at: i).
		tableIndex := self increment: tableIndex bits: minBits].
	"Fill up remaining entries with invalid entries"
	tableStack := OrderedCollection new: 10. "Should be more than enough"
	tableStack addLast: 
		(Array 
			with: minBits	"Number of bits (e.g., depth) for this table"
			with: tableStart	"Start of table"
			with: tableIndex "Next index in table"
			with: minBits	"Number of delta bits encoded in table"
			with: tableSize - valueIndex "Entries remaining in table").
	"Go to next value index"
	valueIndex := valueIndex + 1.
	"Walk over remaining bit lengths and create new subtables"
	minBits+1 to: maxBits do:[:bits|
		numValues := counts at: bits+1.
		[numValues > 0] whileTrue:["Create a new subtable"
			lastTable := tableStack last.
			lastTableStart := lastTable at: 2.
			lastTableIndex := lastTable at: 3.
			deltaBits := bits - (lastTable at: 1).
			"Make up a table of deltaBits size"
			tableSize := 1 bitShift: deltaBits.
			tableStart := tableEnd.
			tableEnd := tableEnd + tableSize.
			[tableEnd > table size ]
				whileTrue:[table := self growHuffmanTable: table].
			"Connect to last table"
			[(table at: lastTableStart + lastTableIndex) = 0] assert."Entry must be unused"
			table at: lastTableStart + lastTableIndex put: (deltaBits bitShift: 24) + tableStart.
			lastTable at: 3 put: (self increment: lastTableIndex bits: (lastTable at: 4)).
			lastTable at: 5 put: (lastTable at: 5) - 1.
			[(lastTable at: 5) >= 0] assert. "Don't exceed tableSize"
			"Store terminal values"
			maxEntries := numValues min: tableSize.
			tableIndex := 0.
			1 to: maxEntries do:[:i|
				table at: tableStart + tableIndex put: (values at: valueIndex).
				valueIndex := valueIndex + 1.
				numValues := numValues - 1.
				tableIndex := self increment: tableIndex bits: deltaBits].
			"Check if we have filled up the current table completely"
			maxEntries = tableSize ifTrue:[
				"Table has been filled. Back up to the last table with space left."
				[tableStack isEmpty not and:[(tableStack last at: 5) = 0]]
						whileTrue:[tableStack removeLast].
			] ifFalse:[
				"Table not yet filled. Put it back on the stack."
				tableStack addLast:
					(Array
						with: bits		"Nr. of bits in this table"
						with: tableStart	"Start of table"
						with: tableIndex "Index in table"
						with: deltaBits	"delta bits of table"
						with: tableSize - maxEntries "Unused entries in table").
			].
		].
	].
	 ^table copyFrom: 1 to: tableEnd-1
]

{ #category : #'huffman trees' }
InflateStream >> decodeDynamicTable: nItems from: aHuffmanTable [
	"Decode the code length of the literal/length and distance table
	in a block compressed with dynamic huffman trees"
	| values index value repCount theValue |
	values := Array new: nItems.
	index := 1.
	theValue := 0.
	[index <= nItems] whileTrue:[
		value := self decodeValueFrom: aHuffmanTable.
		value < 16 ifTrue:[
			"Immediate values"
			theValue := value.
			values at: index put: value.
			index := index+1.
		] ifFalse:[
			"Repeated values"
			value = 16 ifTrue:[
				"Repeat last value"
				repCount := (self nextBits: 2) + 3.
			] ifFalse:[
				"Repeat zero value"
				theValue := 0.
				value = 17 
					ifTrue:[repCount := (self nextBits: 3) + 3]
					ifFalse:[value = 18 
								ifTrue:[repCount := (self nextBits: 7) + 11]
								ifFalse:[^self error:'Invalid bits tree value']]].
			0 to: repCount-1 do:[:i| values at: index+i put: theValue].
			index := index + repCount].
	].
	^values
]

{ #category : #inflating }
InflateStream >> decodeValueFrom: table [
	"Decode the next value in the receiver using the given huffman table."
	| bits bitsNeeded tableIndex value |
	bitsNeeded := (table at: 1) bitShift: -24.	"Initial bits needed"
	tableIndex := 2.							"First real table"
	[bits := self nextSingleBits: bitsNeeded.	"Get bits"
	value := table at: (tableIndex + bits).		"Lookup entry in table"
	(value bitAnd: 16r3F000000) = 0] 			"Check if it is a non-leaf node"
		whileFalse:["Fetch sub table"
			tableIndex := value bitAnd: 16rFFFF.	"Table offset in low 16 bit"
			bitsNeeded := (value bitShift: -24) bitAnd: 255. "Additional bits in high 8 bit"
			bitsNeeded > MaxBits ifTrue:[^self error:'Invalid huffman table entry']].
	^value
]

{ #category : #private }
InflateStream >> decompressAll [
	"Profile the decompression speed"
	[self atEnd] whileFalse:[
		position := readLimit.
		self next "Provokes decompression"
	].
]

{ #category : #inflating }
InflateStream >> decompressBlock: llTable with: dTable [
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
]

{ #category : #'huffman trees' }
InflateStream >> distanceMap [
	"This is used by the fast decompressor"
	^nil
]

{ #category : #private }
InflateStream >> getFirstBuffer [
	"Get the first source buffer after initialization has been done"
	sourceStream == nil ifTrue:[^self].
	source := sourceStream next: 1 << 16. "This is more than enough..."
	sourceLimit := source size.
]

{ #category : #private }
InflateStream >> getNextBlock [
	^self nextBits: 3
]

{ #category : #'huffman trees' }
InflateStream >> growHuffmanTable: table [
	| newTable |
	newTable := table species new: table size * 2.
	newTable replaceFrom: 1 to: table size with: table startingAt: 1.
	^newTable
]

{ #category : #'huffman trees' }
InflateStream >> huffmanTableFrom: aCollection mappedBy: valueMap [
	"Create a new huffman table from the given code lengths.
	Map the actual values by valueMap if it is given.
	See the class comment for a documentation of the huffman
	tables used in this decompressor."
	| counts  values table minBits maxBits |
	minBits := MaxBits + 1.
	maxBits := 0.
	"Count the occurences of each code length and compute minBits and maxBits"
	counts := Array new: MaxBits+1.
	counts atAllPut: 0.
	aCollection do:[:length| 
		length > 0 ifTrue:[
			length < minBits ifTrue:[minBits := length].
			length > maxBits ifTrue:[maxBits := length].
			counts at: length+1 put: (counts at: length+1)+1]].
	maxBits = 0 ifTrue:[^nil]. "Empty huffman table"

	"Assign numerical values to all codes."
	values := self computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits.

	"Map the values if requested"
	self mapValues: values by: valueMap.

	"Create the actual tables"
	table := self createHuffmanTables: values counts: counts from: minBits to: maxBits.

	^table
]

{ #category : #'huffman trees' }
InflateStream >> increment: value bits: nBits [
	"Increment a value of nBits length.
	The fast decompressor will do this differently"
	^value+1
]

{ #category : #'huffman trees' }
InflateStream >> literalLengthMap [
	"This is used by the fast decompressor"
	^nil
]

{ #category : #'huffman trees' }
InflateStream >> mapValues: values by: valueMap [
	| oldValue |
	valueMap ifNil: [^values].
	1 to: values size do:[:i|
		oldValue := values at: i.
		"Note: there may be nil values if not all values are used"
		oldValue 
			ifNil: [^values]
			ifNotNil: [values at: i put: (valueMap at: oldValue+1)]].

]

{ #category : #private }
InflateStream >> moveContentsToFront [
	"Move the decoded contents of the receiver to the front so that we have enough space for decoding more data."
	| delta |
	readLimit > 32768 ifTrue:[
		delta := readLimit - 32767.
		collection 
			replaceFrom: 1 
			to: collection size - delta + 1 
			with: collection 
			startingAt: delta.
		position := position - delta + 1.
		readLimit := readLimit - delta + 1].
]

{ #category : #private }
InflateStream >> moveSourceToFront [
	"Move the encoded contents of the receiver to the front so that we have enough space for decoding more data."
	(sourceStream == nil or:[sourceStream atEnd]) ifTrue:[^self].
	sourcePos > 10000 ifTrue:[
		source 
			replaceFrom: 1 
			to: source size - sourcePos
			with: source 
			startingAt: sourcePos + 1.
		source := sourceStream 
			next: sourcePos 
			into: source 
			startingAt: source size - sourcePos + 1.
		sourcePos := 0.
		sourceLimit := source size].
]

{ #category : #accessing }
InflateStream >> next [
	"Answer the next decompressed object in the Stream represented by the
	receiver."

	position >= readLimit
		ifTrue: [^self pastEndRead]
		ifFalse: [^collection at: (position := position + 1)]
]

{ #category : #accessing }
InflateStream >> next: anInteger [ 
	"Answer the next anInteger elements of my collection.  overriden for simplicity"
	| newArray |

	"try to do it the fast way"
	position + anInteger < readLimit ifTrue: [
		newArray := collection copyFrom: position + 1 to: position + anInteger.
		position := position + anInteger.
		^newArray
	].

	"oh, well..."
	newArray := collection species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: (self next ifNil: [ ^newArray copyFrom: 1 to: index - 1]) ].
	^newArray
]

{ #category : #accessing }
InflateStream >> next: n into: buffer startingAt: startIndex [
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| c numRead count |
	n = 0 ifTrue: [ ^buffer ].
	numRead := 0.
	["Force decompression if necessary"
	(c := self next) == nil 
		ifTrue:[^buffer copyFrom: 1 to: startIndex+numRead-1].
	"Store the first value which provoked decompression"
	buffer at: startIndex + numRead put: c.
	numRead := numRead + 1.
	"After collection has been filled copy as many objects as possible"
	count := (readLimit - position) min: (n - numRead).
	buffer 
		replaceFrom: startIndex + numRead 
		to: startIndex + numRead + count - 1 
		with: collection 
		startingAt: position+1.
	position := position + count.
	numRead := numRead + count.
	numRead = n] whileFalse.
	^buffer
]

{ #category : #'bit access' }
InflateStream >> nextBits: n [
	| bits |
	[bitPos < n] whileTrue:[
		bitBuf := bitBuf + (self nextByte bitShift: bitPos).
		bitPos := bitPos + 8].
	bits := bitBuf bitAnd: (1 bitShift: n)-1.
	bitBuf := bitBuf bitShift: 0 - n.
	bitPos := bitPos - n.
	^bits
]

{ #category : #'bit access' }
InflateStream >> nextByte [
	^source byteAt: (sourcePos := sourcePos + 1)
]

{ #category : #'bit access' }
InflateStream >> nextSingleBits: n [
	| out |
	out := 0.
	1 to: n do:[:i| out := (out bitShift: 1) + (self nextBits: 1)].
	^out
]

{ #category : #initialization }
InflateStream >> on: aCollectionOrStream [
	aCollectionOrStream isStream 
		ifTrue:[
			sourceStream := aCollectionOrStream.
			self getFirstBuffer
		] ifFalse:[
			source := aCollectionOrStream ].
	^self on: source from: 1 to: source size.
]

{ #category : #initialization }
InflateStream >> on: aCollection from: firstIndex to: lastIndex [
	bitBuf := bitPos := 0.
	"The decompression buffer has a size of at 64k,
	since we may have distances up to 32k back and
	repetitions of at most 32k length forward"
	collection := aCollection species new: 1 << 16.
	readLimit := 0. "Not yet initialized"
	position := 0.
	source := aCollection.
	sourceLimit := lastIndex.
	sourcePos := firstIndex-1.
	state := StateNewBlock.
]

{ #category : #private }
InflateStream >> pastEndRead [
	"A client has attempted to read beyond the read limit.
	Check in what state we currently are and perform
	the appropriate action"
	| blockType bp oldLimit |
	state = StateNoMoreData ifTrue:[^nil]. "Get out early if possible"
	"Check if we can move decoded data to front"
	self moveContentsToFront.
	"Check if we can fetch more source data"
	self moveSourceToFront.
	state = StateNewBlock ifTrue:[state := self getNextBlock].
	blockType := state bitShift: -1.
	bp := self bitPosition.
	oldLimit := readLimit.
	self perform: (BlockTypes at: blockType+1).
	"Note: if bit position hasn't advanced then nothing has been decoded."
	bp = self bitPosition 
		ifTrue:[^self primitiveFailed].
	"Update crc for the decoded contents"
	readLimit > oldLimit 
		ifTrue:[crc := self updateCrc: crc from: oldLimit+1 to: readLimit in: collection].
	state = StateNoMoreData ifTrue:[self verifyCrc].
	^self next
]

{ #category : #inflating }
InflateStream >> proceedDynamicBlock [
	self decompressBlock: litTable with: distTable
]

{ #category : #inflating }
InflateStream >> proceedFixedBlock [
	self decompressBlock: litTable with: distTable
]

{ #category : #inflating }
InflateStream >> proceedStoredBlock [
	"Proceed decompressing a stored (e.g., uncompressed) block"
	| length decoded |
	"Literal table must be nil for a stored block"
	litTable == nil ifFalse:[^self error:'Bad state'].
	length := distTable.
	[length > 0 and:[readLimit < collection size and:[sourcePos < sourceLimit]]] 
		whileTrue:[
			collection at: (readLimit := readLimit + 1) put: 
				(source at: (sourcePos := sourcePos + 1)).
			length := length - 1].
	length = 0 ifTrue:[state := state bitAnd: StateNoMoreData].
	decoded := length - distTable.
	distTable := length.
	^decoded
]

{ #category : #inflating }
InflateStream >> processDynamicBlock [
	| nLit nDist nLen codeLength lengthTable bits |
	nLit := (self nextBits: 5) + 257.
	nDist := (self nextBits: 5) + 1.
	nLen := (self nextBits: 4) + 4.
	codeLength := Array new: 19.
	codeLength atAllPut: 0.
	1 to: nLen do:[:i|
		bits := #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) at: i.
		codeLength at: bits+1 put: (self nextBits: 3).
	].
	lengthTable := self huffmanTableFrom: codeLength mappedBy: nil.
	"RFC 1951: In other words, all code lengths form a single sequence..."
	codeLength := self decodeDynamicTable: nLit+nDist from: lengthTable.
	litTable := self 
				huffmanTableFrom: (codeLength copyFrom: 1 to: nLit)
				mappedBy: self literalLengthMap.
	distTable := self 
				huffmanTableFrom: (codeLength copyFrom: nLit+1 to: codeLength size)
				mappedBy: self distanceMap.
	state := state bitOr: BlockProceedBit.
	self proceedDynamicBlock.
]

{ #category : #inflating }
InflateStream >> processFixedBlock [
	litTable := self 
				huffmanTableFrom: FixedLitCodes
				mappedBy: self literalLengthMap.
	distTable := self 
				huffmanTableFrom: FixedDistCodes
				mappedBy: self distanceMap.
	state := state bitOr: BlockProceedBit.
	self proceedFixedBlock.
]

{ #category : #inflating }
InflateStream >> processStoredBlock [
	| chkSum length |
	"Skip to byte boundary"
	self nextBits: (bitPos bitAnd: 7).
	length := self nextBits: 16.
	chkSum := self nextBits: 16.
	(chkSum bitXor: 16rFFFF) = length
		ifFalse:[^self error:'Bad block length'].
	litTable := nil.
	distTable := length.
	state := state bitOr: BlockProceedBit.
	^self proceedStoredBlock
]

{ #category : #accessing }
InflateStream >> readInto: buffer startingAt: startIndex count: n [
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	| c numRead count |
	n = 0 ifTrue: [ ^n ].
	numRead := 0.
	["Force decompression if necessary"
	(c := self next) == nil 
		ifTrue: [^numRead].
	"Store the first value which provoked decompression"
	buffer at: startIndex + numRead put: c.
	numRead := numRead + 1.
	"After collection has been filled copy as many objects as possible"
	count := (readLimit - position) min: (n - numRead).
	buffer 
		replaceFrom: startIndex + numRead 
		to: startIndex + numRead + count - 1 
		with: collection 
		startingAt: position+1.
	position := position + count.
	numRead := numRead + count.
	numRead = n] whileFalse.
	^n
]

{ #category : #initialization }
InflateStream >> reset [
	"Position zero - nothing decoded yet"
	position := readLimit := 0.
	sourcePos := 0.
	bitBuf := bitPos := 0.
	state := 0.
]

{ #category : #accessing }
InflateStream >> size [
	"This is a compressed stream - we don't know the size beforehand"
	^self shouldNotImplement
]

{ #category : #accessing }
InflateStream >> sourceLimit [
	^sourceLimit
]

{ #category : #positioning }
InflateStream >> sourcePosition [
	^sourcePos
]

{ #category : #accessing }
InflateStream >> sourceStream [
	^sourceStream
]

{ #category : #accessing }
InflateStream >> upTo: anObject [ 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := (collection species new: 100) writeStream.
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents
]

{ #category : #accessing }
InflateStream >> upToEnd [
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream buffer |
	buffer := collection species new: 1000.
	newStream := (collection species new: 100) writeStream.
	[self atEnd] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].
	^ newStream contents
]

{ #category : #crc }
InflateStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	"Answer an updated CRC for the range of bytes in aCollection.
	Subclasses can implement the appropriate means for the check sum they wish to use."
	^oldCrc
]

{ #category : #crc }
InflateStream >> verifyCrc [
	"Verify the crc checksum in the input"
]
