"
A ZIP write stream
"
Class {
	#name : #ZipWriteStream,
	#superclass : #DeflateStream,
	#instVars : [
		'literals',
		'distances',
		'literalFreq',
		'distanceFreq',
		'litCount',
		'matchCount',
		'encoder',
		'crc',
		'crcPosition',
		'bytesWritten'
	],
	#classVars : [
		'VerboseLevel'
	],
	#pools : [
		'ZipConstants'
	],
	#category : #'Compression-Streams'
}

{ #category : #accessing }
ZipWriteStream class >> baseDistance [
	^BaseDistance
]

{ #category : #accessing }
ZipWriteStream class >> baseLength [
	^BaseLength
]

{ #category : #accessing }
ZipWriteStream class >> distanceCodes [
	^DistanceCodes
]

{ #category : #accessing }
ZipWriteStream class >> extraDistanceBits [
	^ExtraDistanceBits
]

{ #category : #accessing }
ZipWriteStream class >> extraLengthBits [
	^ExtraLengthBits
]

{ #category : #initialization }
ZipWriteStream class >> initialize [
	"ZipWriteStream initialize"
	VerboseLevel := 0
]

{ #category : #accessing }
ZipWriteStream class >> matchLengthCodes [
	^MatchLengthCodes
]

{ #category : #accessing }
ZipWriteStream class >> maxDistanceCodes [
	^MaxDistCodes
]

{ #category : #accessing }
ZipWriteStream class >> maxLiteralCodes [
	^MaxLiteralCodes
]

{ #category : #'regression test' }
ZipWriteStream class >> printRegressionStats: stats from: fd [
	| raw compressed numFiles |
	raw := stats at: #rawSize ifAbsent: [ 0 ].
	raw = 0
		ifTrue: [ ^ self ].
	compressed := stats at: #compressedSize ifAbsent: [ 0 ].
	numFiles := stats at: #numFiles ifAbsent: [ 0 ].
	
	SystemNotification signal: (String streamContents: [ :aStream |
		aStream nextPutAll: fd fullName asString; cr.
		aStream tab; nextPutAll: 'Files compressed: '; nextPutAll: numFiles asStringWithCommas; cr.
		aStream tab; nextPutAll: 'Bytes compressed: '; nextPutAll: raw asStringWithCommas; cr.
		aStream tab; nextPutAll: 'Avg. compression ratio: '; nextPutAll: (compressed / raw asFloat * 100.0 truncateTo: 0.01) asString ])
]

{ #category : #'regression test' }
ZipWriteStream class >> regressionCompress: aFile into: tempFile notifiying: progressBar stats: stats [
	"Compress aFile into tempFile"
	| zip encoded buffer |
	aFile binary.
	aFile position: 0.
	tempFile binary.
	buffer := ByteArray new: 4096.
	zip := self on: (ByteArray new: 10000).
	encoded := zip encodedStream.
	[aFile atEnd] whileFalse:[
		progressBar current: aFile position.
		zip nextPutAll: (aFile nextInto: buffer).
		encoded position > 0 ifTrue:[
			tempFile nextPutAll: encoded contents.
			encoded position: 0]].
	zip close.
	tempFile nextPutAll: encoded contents.
	^true
]

{ #category : #crc }
ZipWriteStream class >> updateCrc: oldCrc from: start to: stop in: aCollection [
	^ CRC update: oldCrc from: start to: stop in: aCollection
]

{ #category : #'open/close' }
ZipWriteStream >> close [
	self deflateBlock.
	self flushBlock: true.
	encoder close.
]

{ #category : #accessing }
ZipWriteStream >> crc [
	^crc
]

{ #category : #deflating }
ZipWriteStream >> deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch [
	"^DeflatePlugin doPrimitive:#primitiveDeflateBlock"
	<primitive: 'primitiveDeflateBlock' module: 'ZipPlugin'>
	^super deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> dynamicBlockSizeFor: lTree and: dTree using: blTree and: blFreq [
	"Compute the length for the current block using dynamic huffman trees"
	| bits index extra treeBits freq |
	bits := 3 "block type" + 5 "literal codes length" + 5 "distance codes length".

	"Compute the # of bits for sending the bit length tree"
	treeBits := 4. "Max index for bit length tree"
	index := MaxBitLengthCodes.
	[index >= 4] whileTrue:[
		(index = 4 or:[(blFreq at: (BitLengthOrder at: index)+1) > 0])
			ifTrue:[treeBits := treeBits + (index * 3).
					index := -1]
			ifFalse:[index := index - 1]].

	"Compute the # of bits for sending the literal/distance tree.
	Note: The frequency are already stored in the blTree"
	0 to: 15 do:[:i| "First, the non-repeating values"
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[treeBits := treeBits + (freq * (blTree bitLengthAt: i))]].
	"Now the repeating values"
	(Repeat3To6 to: Repeat11To138) with: #(2 3 7) do:[:i :addl|
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[
			treeBits := treeBits + (freq * ((blTree bitLengthAt: i) + addl "addl bits"))]].
	VerboseLevel > 1 ifTrue:[
		SystemNotification signal: ('[',  treeBits asString, ' bits for dynamic tree]')].
	bits := bits + treeBits.

	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		freq := literalFreq at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * (lTree bitLengthAt: i))]].
	NumLiterals+1 to: lTree maxCode do:[:i| "encoding of match lengths"
		freq := literalFreq at: i+1.
		extra := ExtraLengthBits at: i-NumLiterals.
		freq > 0 ifTrue:[bits := bits + (freq * ((lTree bitLengthAt: i) + extra))]].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		freq := distanceFreq at: i+1.
		extra := ExtraDistanceBits at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * ((dTree bitLengthAt: i) + extra))]].

	^bits
]

{ #category : #encoding }
ZipWriteStream >> encodeLiteral: lit [
	"Encode the given literal"
	litCount := litCount + 1.
	literals at: litCount put: lit.
	distances at: litCount put: 0.
	literalFreq at: lit+1 put: (literalFreq at: lit+1) + 1.
	^self shouldFlush
]

{ #category : #encoding }
ZipWriteStream >> encodeMatch: length distance: dist [
	"Encode the given match of length length starting at dist bytes ahead"
	| literal distance |
	dist > 0 
		ifFalse:[^self error:'Distance must be positive'].
	length < MinMatch 
		ifTrue:[^self error:'Match length must be at least ', MinMatch printString].
	litCount := litCount + 1.
	matchCount := matchCount + 1.
	literals at: litCount put: length - MinMatch.
	distances at: litCount put: dist.
	literal := (MatchLengthCodes at: length - MinMatch + 1).
	literalFreq at: literal+1 put: (literalFreq at: literal+1) + 1.
	dist < 257
		ifTrue:[distance := DistanceCodes at: dist]
		ifFalse:[distance := DistanceCodes at: 257 + (dist - 1 bitShift: -7)].
	distanceFreq at: distance+1 put: (distanceFreq at: distance+1) + 1.
	^self shouldFlush
]

{ #category : #accessing }
ZipWriteStream >> encodedStream [
	^encoder encodedStream
]

{ #category : #initialization }
ZipWriteStream >> finish [
	"Finish pending operation. Do not close output stream."
	self deflateBlock.
	self flushBlock: true.
	encoder flush.
]

{ #category : #'fixed blocks' }
ZipWriteStream >> fixedBlockSizeFor: lTree and: dTree [
	"Compute the length for the current block using fixed huffman trees"
	| bits extra |
	bits := 3 "block type".
	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		bits := bits + ((literalFreq at: i+1) * (FixedLiteralTree bitLengthAt: i))].
	NumLiterals+1 to: lTree maxCode+1 do:[:i| "Encoding of match lengths"
		extra := ExtraLengthBits at: i-NumLiterals.
		bits := bits + ((literalFreq at: i+1) * ((FixedLiteralTree bitLengthAt: i) + extra))].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		extra := ExtraDistanceBits at: i+1.
		bits := bits + ((distanceFreq at: i+1) * ((FixedDistanceTree bitLengthAt: i) + extra))].

	^bits
]

{ #category : #encoding }
ZipWriteStream >> flushBlock [
	^self flushBlock: false
]

{ #category : #encoding }
ZipWriteStream >> flushBlock: lastBlock [
	"Send the current block"

	| lastFlag bitsRequired method bitsSent storedLength fixedLength dynamicLength blTree lTree dTree blBits blFreq |
	lastFlag := lastBlock
		ifTrue: [ 1 ]
		ifFalse: [ 0 ].

	"Compute the literal/length and distance tree"
	lTree := ZipEncoderTree buildTreeFrom: literalFreq maxDepth: MaxBits.
	dTree := ZipEncoderTree buildTreeFrom: distanceFreq maxDepth: MaxBits.

	"Compute the bit length tree"
	blBits := lTree bitLengths , dTree bitLengths.
	blFreq := WordArray new: MaxBitLengthCodes.
	self scanBitLengths: blBits into: blFreq.
	blTree := ZipEncoderTree
		buildTreeFrom: blFreq
		maxDepth: MaxBitLengthBits.

	"Compute the bit length for the current block.
	Note: Most of this could be computed on the fly but it's getting
	really ugly in this case so we do it afterwards."
	storedLength := self storedBlockSize.
	fixedLength := self fixedBlockSizeFor: lTree and: dTree.
	dynamicLength := self
		dynamicBlockSizeFor: lTree
		and: dTree
		using: blTree
		and: blFreq.
	VerboseLevel > 1
		ifTrue: [ 
			SystemNotification signal:
					(String
						streamContents: [ :stream | 
							stream
								nextPutAll: 'Block sizes (S/F/D):' space;
								print: storedLength // 8;
								nextPut: $/;
								print: fixedLength // 8;
								nextPut: $/;
								print: dynamicLength // 8;
								space;
								endEntry ]) ].

	"Check which method to use"
	method := self forcedMethod.
	method
		ifNil: [ method := (storedLength < fixedLength
				and: [ storedLength < dynamicLength ])
				ifTrue: [ #stored ]
				ifFalse: [ fixedLength < dynamicLength
						ifTrue: [ #fixed ]
						ifFalse: [ #dynamic ] ] ].
	(method == #stored and: [ blockStart < 0 ])
		ifTrue: [ "Cannot use #stored if the block is not available"
			method := fixedLength < dynamicLength
				ifTrue: [ #fixed ]
				ifFalse: [ #dynamic ] ].
	bitsSent := encoder bitPosition.	"# of bits sent before this block"
	bitsRequired := nil.
	method == #stored
		ifTrue: [ VerboseLevel > 0
				ifTrue: [ SystemNotification signal: 'S' ].
			bitsRequired := storedLength.
			encoder nextBits: 3 put: (StoredBlock << 1) + lastFlag.
			self sendStoredBlock ].
	method == #fixed
		ifTrue: [ VerboseLevel > 0
				ifTrue: [ SystemNotification signal: 'F' ].
			bitsRequired := fixedLength.
			encoder nextBits: 3 put: (FixedBlock << 1) + lastFlag.
			self sendFixedBlock ].
	method == #dynamic
		ifTrue: [ VerboseLevel > 0
				ifTrue: [ SystemNotification signal: 'D' ].
			bitsRequired := dynamicLength.
			encoder nextBits: 3 put: (DynamicBlock << 1) + lastFlag.
			self
				sendDynamicBlock: blTree
				literalTree: lTree
				distanceTree: dTree
				bitLengths: blBits ].
	bitsRequired = (encoder bitPosition - bitsSent)
		ifFalse: [ self error: 'Bits size mismatch' ].
	lastBlock
		ifTrue: [ self release ]
		ifFalse: [ self initializeNewBlock ]
]

{ #category : #accessing }
ZipWriteStream >> forcedMethod [
	"Return a symbol describing an enforced method or nil if the method should
	be chosen adaptively. Valid symbols are
		#stored	- store blocks (do not compress)
		#fixed	- use fixed huffman trees
		#dynamic	- use dynamic huffman trees."
	^nil
]

{ #category : #initialization }
ZipWriteStream >> initialize [
	super initialize.
	literals := ByteArray new: WindowSize.
	distances := WordArray new: WindowSize.
	literalFreq := WordArray new: MaxLiteralCodes.
	distanceFreq := WordArray new: MaxDistCodes.
	self initializeNewBlock.

]

{ #category : #initialization }
ZipWriteStream >> initializeNewBlock [
	"Initialize the encoder for a new block of data"
	literalFreq atAllPut: 0.
	distanceFreq atAllPut: 0.
	literalFreq at: EndBlock+1 put: 1.
	litCount := 0.
	matchCount := 0.
]

{ #category : #private }
ZipWriteStream >> moveContentsToFront [
	"Need to update crc here"
	self updateCrc.
	super moveContentsToFront.
	crcPosition := position + 1.
]

{ #category : #initialization }
ZipWriteStream >> on: aCollectionOrStream [
	crc := 16rFFFFFFFF.
	crcPosition := 1.
	bytesWritten := 0.
	encoder := ZipEncoder on: aCollectionOrStream.
	encoder isBinary
		ifTrue:[super on: ByteArray new]
		ifFalse:[super on: String new].
	self writeHeader.

]

{ #category : #initialization }
ZipWriteStream >> release [
	"We're done with compression. Do some cleanup."
	literals := distances := literalFreq := distanceFreq := nil.
	self updateCrc.
	encoder flushBits.
	self writeFooter.
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> scanBitLength: bitLength repeatCount: repeatCount into: anArray [
	"Update the frequency for the aTree based on the given values"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			anArray at: Repeat11To138+1 put: (anArray at: Repeat11To138+1) + 1.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			anArray at: Repeat3To10+1 put: (anArray at: Repeat3To10+1) + 1.
			count := (count - 10) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	] ifFalse:[
		anArray at: bitLength+1 put: (anArray at: bitLength+1) + 1.
		count := count - 1.
		[count >= 3] whileTrue:[
			anArray at: Repeat3To6+1 put: (anArray at: Repeat3To6+1) + 1.
			count := (count - 6) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	].
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> scanBitLengths: bits into: anArray [
	"Scan the trees and determine the frequency of the bit lengths.
	For repeating codes, emit a repeat count."
	| lastValue lastCount value |
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self scanBitLength: lastValue repeatCount: lastCount into: anArray.
					lastValue := value.
					lastCount := 1]].
	self scanBitLength: lastValue repeatCount: lastCount into: anArray.
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendBitLength: bitLength repeatCount: repeatCount tree: aTree [
	"Send the given bitLength, repeating repeatCount times"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			self sendBitLength: Repeat11To138 tree: aTree.
			encoder nextBits: 7 put: (count min: 138) - 11.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To10 tree: aTree.
			encoder nextBits: 3 put: (count min: 10) - 3.
			count := (count - 10) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	] ifFalse:[
		self sendBitLength: bitLength tree: aTree.
		count := count - 1.
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To6 tree: aTree.
			encoder nextBits: 2 put: (count min: 6) - 3.
			count := (count - 6) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	].
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendBitLength: bitLength tree: aTree [
	"Send the given bitLength"
	encoder nextBits: (aTree bitLengthAt: bitLength) 
		put: (aTree codeAt: bitLength).
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendBitLengthTree: blTree [
	"Send the bit length tree"
	| blIndex bitLength |
	MaxBitLengthCodes to: 4 by: -1 do:[:maxIndex|
		blIndex := BitLengthOrder at: maxIndex.
		bitLength := blIndex <= blTree maxCode 
			ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
		(maxIndex = 4 or:[bitLength > 0]) ifTrue:[
			encoder nextBits: 4 put: maxIndex - 4.
			1 to: maxIndex do:[:j|
				blIndex := BitLengthOrder at: j.
				bitLength := blIndex <= blTree maxCode 
					ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
				encoder nextBits: 3 put: bitLength].
			^self]].
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendCompressedBlock: litTree with: distTree [
	"Send the current block using the encodings from the given literal/length and distance tree"
	| sum |
	sum := encoder
			sendBlock: (ReadStream on: literals from: 1 to: litCount)
			with: (ReadStream on: distances from: 1 to: litCount)
			with: litTree
			with: distTree.
	sum = (blockPosition - blockStart) ifFalse:[self error:'Wrong number of bytes'].
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendDynamicBlock: blTree literalTree: lTree distanceTree: dTree bitLengths: bits [
	"Send a block using dynamic huffman trees"
	self sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits.
	self sendCompressedBlock: lTree with: dTree.
]

{ #category : #'fixed blocks' }
ZipWriteStream >> sendFixedBlock [
	"Send a block using fixed huffman trees"
	self sendCompressedBlock: FixedLiteralTree with: FixedDistanceTree.
]

{ #category : #'dynamic blocks' }
ZipWriteStream >> sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits [
	"Send all the trees needed for dynamic huffman tree encoding"
	| lastValue lastCount value |
	encoder nextBits: 5 put: (lTree maxCode - 256).
	encoder nextBits: 5 put: (dTree maxCode).
	self sendBitLengthTree: blTree.
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
					lastValue := value.
					lastCount := 1]].
	self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
]

{ #category : #'stored blocks' }
ZipWriteStream >> sendStoredBlock [
	"Send an uncompressed block"
	| inBytes |
	inBytes := blockPosition - blockStart.
	encoder flushBits. "Skip to byte boundary"
	encoder nextBits: 16 put: inBytes.
	encoder nextBits: 16 put: (inBytes bitXor: 16rFFFF).
	encoder flushBits.
	1 to: inBytes do:[:i|
		encoder nextBytePut: (collection byteAt: blockStart+i)].
]

{ #category : #encoding }
ZipWriteStream >> shouldFlush [
	"Check if we should flush the current block.
	Flushing can be useful if the input characteristics change."
	| nLits |
	litCount = literals size ifTrue:[^true]. "We *must* flush"
	(litCount bitAnd: 16rFFF) = 0 ifFalse:[^false]. "Only check every N kbytes"
	matchCount * 10 <= litCount ifTrue:[
		"This is basically random data. 
		There is no need to flush early since the overhead
		for encoding the trees will add to the overall size"
		^false].
	"Try to adapt to the input data.
	We flush if the ratio between matches and literals
	changes beyound a certain threshold"
	nLits := litCount - matchCount.
	nLits <= matchCount ifTrue:[^false]. "whow! so many matches"
	^nLits * 4 <= matchCount
]

{ #category : #'stored blocks' }
ZipWriteStream >> storedBlockSize [
	"Compute the length for the current block when stored as is"
	^3 "block type bits" 
		+ (8 - (encoder bitPosition + 3 bitAnd: 7) bitAnd: 7)"skipped bits to byte boundary"
			+ 32 "byte length + chksum" 
				+ (blockPosition - blockStart * 8) "actual data bits".
]

{ #category : #private }
ZipWriteStream >> updateCrc [
	crcPosition <= position ifTrue:[
		bytesWritten := bytesWritten + position - crcPosition + 1.
		crc := self updateCrc: crc from: crcPosition to: position in: collection.
		crcPosition := position + 1].
]

{ #category : #private }
ZipWriteStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	^self class updateCrc: oldCrc from: start to: stop in: aCollection
]

{ #category : #initialization }
ZipWriteStream >> writeFooter [
	"Write footer information if necessary"
	crc := crc bitXor: 16rFFFFFFFF.
]

{ #category : #initialization }
ZipWriteStream >> writeHeader [
	"Write header information if necessary"
]
