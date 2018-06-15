Class {
	#name : #NeoLzwGifDecoder,
	#superclass : #Object,
	#instVars : [
		'suffixTable',
		'prefixTable',
		'eoiCode',
		'clearCode',
		'bitMask',
		'codeSize',
		'minimumCodeSize',
		'maxCode',
		'nextAvailableCode',
		'numLeftoverBits',
		'codeStream',
		'codeStreamBuffer',
		'outBlock'
	],
	#category : #'Images-Animated'
}

{ #category : #private }
NeoLzwGifDecoder >> checkCodeSize [
	"Ensure that the next available code to enter
	into the table is not equal to the bitMask.
	If it is, we increment the code size and update the
	mask value."
	nextAvailableCode := nextAvailableCode + 1.
	(nextAvailableCode bitAnd: bitMask) = 0
		ifTrue: [ 
			"GIF89a specifies a 'deferred' clearCode
			implementation, which means we keep going
			with the current table even if its full but
			a clear hasn't been found. We use the max
			code size at that point."
			nextAvailableCode >= maxCode ifTrue: [ ^ self ].
			codeSize := codeSize + 1.
			bitMask := bitMask + nextAvailableCode ].
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> codeStream: aReadableStream [ 
	| chunkSize |
	codeStream := aReadableStream.
	chunkSize := codeStream next.
	codeStreamBuffer := ReadStream on: (codeStream next: chunkSize).
]

{ #category : #api }
NeoLzwGifDecoder >> decode [
	| currentCode prevCode outStream |
	self initializeTables.
	outStream := WriteStream with: (ByteArray new).
	numLeftoverBits := 0.
	currentCode := self nextCode.
	currentCode = clearCode ifFalse: [ ^ self error: 'First code on the stream should always be the clear code!' ].
	
	"The first real code simply gets output
	onto the stream, then we enter the loop"
	currentCode := self nextCode.
	"outStream nextPut: currentCode."
	self writeBit: currentCode on: outStream.
	prevCode := currentCode.
	currentCode := self nextCode.
	[ currentCode = eoiCode ] whileFalse: [ 
		currentCode = clearCode
			ifTrue: [ 
				Transcript show: 'CLEAR CODE'; cr.
				self initializeTables.
				currentCode := self nextCode.
				"outStream nextPut: (suffixTable at: (currentCode + 1))."
				self
					writeBit: (suffixTable at: (currentCode + 1))
					on: outStream.
				prevCode := nil ]
			ifFalse: [ self handleCode: currentCode withPreviousCode: prevCode on: outStream ].
		prevCode := currentCode.
		currentCode := self nextCode.
		currentCode = eoiCode ifTrue: [ Transcript show: 'EOICODE'; cr ] ].
	^ outStream contents.

	
	
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> handleCode: anInteger withPreviousCode: prevInteger on: aWriteStream [
	"Check for the code in the tables
	and perform the appropriate LZW action"
	| first searchIndex searchStack |
	"The code already exists in the table"
	anInteger < nextAvailableCode
		ifTrue: [ 
			anInteger < clearCode
				"If it's less than the clearCode
				then it is one of the original entries
				and we can simply use the suffix"
				ifTrue: [ 
					first := (suffixTable at: (anInteger + 1)).
					"aWriteStream nextPut: first."
					self writeBit: first on: aWriteStream.
					"prefixTable at: (nextAvailableCode + 1) put: prevInteger.
					suffixTable at: (nextAvailableCode + 1) put: first" ]
				"Otherwise we need to loop backwards along
				the prefix index values and take the suffix each
				time"
				ifFalse: [ 
					searchStack := Stack new.
					searchIndex := anInteger + 1.
					[ searchIndex > clearCode ] whileTrue: [ 
						searchStack push: (suffixTable at: searchIndex).
						searchIndex := (prefixTable at: searchIndex) + 1 ].
					searchStack push: (suffixTable at: searchIndex).
					first := searchStack first.
					searchStack do: [ :int |
						"aWriteStream nextPut: int"
						self writeBit: int on: aWriteStream ] ]. 
			]
		ifFalse: [ 
			"Here, the incoming code is not yet in the code tables"
			prevInteger < clearCode
				ifTrue: [ 
					first := (suffixTable at: (prevInteger + 1)).
					"aWriteStream
						nextPut: first;
						nextPut: first."
					self
						writeBit: first on: aWriteStream;
						writeBit: first on: aWriteStream.
					 ]
				ifFalse: [ 
					searchStack := Stack new.
					searchIndex := prevInteger + 1.
					[ searchIndex > clearCode ] whileTrue: [ 
						searchStack push: (suffixTable at: searchIndex).
						searchIndex := (prefixTable at: searchIndex) + 1 ].
					searchStack push: (suffixTable at: searchIndex).
					first := searchStack first.
					searchStack do: [ :int |
						"aWriteStream nextPut: int"
						self writeBit: int on: aWriteStream ].
					"aWriteStream nextPut: first"
					self writeBit: first on: aWriteStream ]. 
			].
		"We add prevCode and the new
		suffix to a new entry in the code table, but
		only if we aren't at the max. NOTE: due to
		GIF 89a spec's 'deferred clear', if you get to
		the maxCode and haven't seen a clear, you stop
		writing to the tables but continue querying."
		nextAvailableCode >= maxCode
			ifFalse: [ 
				suffixTable at: (nextAvailableCode + 1) put: first.
				prefixTable at: (nextAvailableCode + 1) put: prevInteger ].
		self checkCodeSize.
]

{ #category : #initialization }
NeoLzwGifDecoder >> initializeTables [
	"The two arrays are our lookup tables.
	We do this instead of Dictionaries because
	the method is much, much faster"
	prefixTable := Array new: (maxCode).
	suffixTable := Array new: (maxCode).
	
	"The initial code size and mask settings
	also get reinitialized each time"
	codeSize := minimumCodeSize + 1.
	clearCode := (1 bitShift: minimumCodeSize).
	eoiCode := clearCode + 1.
	nextAvailableCode := clearCode + 2.
	bitMask := (1 bitShift: codeSize) - 1.
	
	"Fill the tables with the initial values"
	1 to: clearCode do: [ :n |
		prefixTable at: n put: (n - 1).
		suffixTable at: n put: (n - 1) ].
	
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> maxCode: anInteger [ 
	maxCode := anInteger
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> minimumCodeSize: anInteger [ 
	minimumCodeSize := anInteger
]

{ #category : #'private-packing' }
NeoLzwGifDecoder >> nextByte [
	| nextChunkSize |
	codeStreamBuffer atEnd
		ifTrue: [ 
			nextChunkSize := codeStream next.
			nextChunkSize = 0 ifTrue: [ ^ self error: 'Next chunk size was 0!' ].
			codeStreamBuffer := (codeStream next: nextChunkSize) readStream ].
	^ codeStreamBuffer next.
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> nextCode [
	| integer numBitsRead newRemainder shiftAmount byte |
	"Retrieve the next code of codeSize bits.
	Store the remaining bits etc for later computation"
	integer := 0.
	numLeftoverBits = 0
		ifTrue: [ 
			numBitsRead := 8.
			shiftAmount := 0 ]
		ifFalse: [ 
			numBitsRead := numLeftoverBits.
			shiftAmount := numLeftoverBits - 8 ].
		[ numBitsRead < codeSize ] whileTrue: [ 
			byte := self nextByte.
			byte == nil ifTrue: [ ^ eoiCode ].
			integer := integer + (byte bitShift: shiftAmount).
			shiftAmount := shiftAmount + 8.
			numBitsRead := numBitsRead + 8 ].
		(newRemainder := numBitsRead - codeSize) = 0
			ifTrue: [ byte := self nextByte ]
			ifFalse: [ byte := self peekByte ].
		byte == nil ifTrue: [ ^ eoiCode ].
		numLeftoverBits := newRemainder.
		^ integer + (byte bitShift: shiftAmount) bitAnd: bitMask.
]

{ #category : #'as yet unclassified' }
NeoLzwGifDecoder >> onDecodedBit: aBlock [
	"This block will be executed once each time a new
	value is decoded from the stream, with the value
	as the sole argument passed to the block"
	outBlock := aBlock
]

{ #category : #'private-packing' }
NeoLzwGifDecoder >> peekByte [
	| nextChunkSize |
	codeStreamBuffer atEnd
		ifTrue: [ 
			nextChunkSize := codeStream next.
			nextChunkSize = 0 ifTrue: [ ^ self error: 'Next chunk size is 0!' ].
			codeStreamBuffer := (codeStream next: nextChunkSize) readStream ].
	^ codeStreamBuffer peek.
]

{ #category : #writing }
NeoLzwGifDecoder >> writeBit: anInteger on: aWriteStream [
	"Write the incoming decoded value onto a
	writestream. If I have an outBlock set, 
	send this value also"
	aWriteStream nextPut: anInteger.
	outBlock ifNil: [ ^ self ].
	outBlock value: anInteger.
]
