"
A ZIP encoder
"
Class {
	#name : #ZipEncoder,
	#superclass : #WriteStream,
	#instVars : [
		'bitBuffer',
		'bitPosition',
		'encodedStream'
	],
	#pools : [
		'ZipConstants'
	],
	#category : #Compression-Streams
}

{ #category : #positioning }
ZipEncoder >> bitPosition [
	^encodedStream position + position * 8 + bitPosition.
]

{ #category : #open/close }
ZipEncoder >> close [
	self flush.
	encodedStream close.
]

{ #category : #initialization }
ZipEncoder >> commit [
	encodedStream next: position putAll: collection.
	position := readLimit := 0.
]

{ #category : #accessing }
ZipEncoder >> encodedStream [
	^encodedStream
]

{ #category : #flushing }
ZipEncoder >> flush [
	self flushBits.
	self commit.
]

{ #category : #flushing }
ZipEncoder >> flushBits [
	"Flush currently unsent bits"
	[bitPosition > 0] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].
	bitPosition := 0.
]

{ #category : #accessing }
ZipEncoder >> nextBits: nBits put: value [
	"Store a value of nBits"
	"self assert:[value >= 0 and:[(1 bitShift: nBits) > value]]."
	bitBuffer := bitBuffer bitOr: (value bitShift: bitPosition).
	bitPosition := bitPosition + nBits.
	[bitPosition >= 8] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].
]

{ #category : #accessing }
ZipEncoder >> nextBytePut: anObject [ 
	"Primitive. Insert the argument at the next position in the Stream
	represented by the receiver. Fail if the collection of this stream is not an
	Array or a String. Fail if the stream is positioned at its end, or if the
	position is out of bounds in the collection. Fail if the argument is not
	of the right type for the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 66>
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: 
			[position := position + 1.
			^collection byteAt: position put: anObject]
]

{ #category : #initialization }
ZipEncoder >> on: aCollectionOrStream [
	aCollectionOrStream isStream 
		ifTrue:[encodedStream := aCollectionOrStream]
		ifFalse:[	encodedStream := aCollectionOrStream writeStream].
	encodedStream isBinary
		ifTrue:[super on: (ByteArray new: 4096)]
		ifFalse:[super on: (String new: 4096)].
	bitPosition := bitBuffer := 0.
]

{ #category : #private }
ZipEncoder >> pastEndPut: anObject [
	"Flush the current buffer and store the new object at the beginning"
	self commit.
	^self nextBytePut: anObject asInteger
]

{ #category : #private }
ZipEncoder >> privateSendBlock: literalStream with: distanceStream with: litTree with: distTree [
	"Send the current block using the encodings from the given literal/length and distance tree"
	| lit dist code extra sum |
	<primitive: 'primitiveZipSendBlock' module: 'ZipPlugin'>
	sum := 0.
	[lit := literalStream next.
	dist := distanceStream next.
	lit == nil] whileFalse:[
		dist = 0 ifTrue:["lit is a literal"
			sum := sum + 1.
			self nextBits: (litTree bitLengthAt: lit)
				put: (litTree codeAt: lit).
		] ifFalse:["lit is match length"
			sum := sum + lit + MinMatch.
			code := (MatchLengthCodes at: lit + 1).
			self nextBits: (litTree bitLengthAt: code)
				put: (litTree codeAt: code).
			extra := ExtraLengthBits at: code-NumLiterals.
			extra = 0 ifFalse:[
				lit := lit - (BaseLength at: code-NumLiterals).
				self nextBits: extra put: lit.
			].
			dist := dist - 1.
			dist < 256
				ifTrue:[code := DistanceCodes at: dist + 1]
				ifFalse:[code := DistanceCodes at: 257 + (dist bitShift: -7)].
			"self assert:[code < MaxDistCodes]."
			self nextBits: (distTree bitLengthAt: code)
				put: (distTree codeAt: code).
			extra := ExtraDistanceBits at: code+1.
			extra = 0 ifFalse:[
				dist := dist - (BaseDistance at: code+1).
				self nextBits: extra put: dist.
			].
		].
	].
	^sum
]

{ #category : #'block encoding' }
ZipEncoder >> sendBlock: literalStream with: distanceStream with: litTree with: distTree [
	"Send the current block using the encodings from the given literal/length and distance tree"
	| result |
	result := 0.
	[literalStream atEnd] whileFalse:[
		result := result + (self privateSendBlock: literalStream
						with: distanceStream with: litTree with: distTree).
		self commit.
	].
	self nextBits: (litTree bitLengthAt: EndBlock) put: (litTree codeAt: EndBlock).
	^result
]
