"
I implement the modified Lempel-Ziv-Welch (LZW) algorithm for lossless GIF decompression. My primary purpose is to decode streams of bytes that have been encoded with this modified version of LZW as used in the GIF standard.

My instances require, at minimum, a maximum code size (via #maxCode:), a minimum code size (via #minimumCodeSize:), and of course a stream of bytes to decode (via #codeStream:). Once these are set, implementors can simply send the #decode message, which will respond with a decoded ByteArray.

Optionally, implementors can make use of the #onDecodedBit: message, which takes a Block with a single argument corresponding to a decoded bit. This Block is called each time a complete byte/bit-level value for the bitmap has been decoded.

For an example of use, see GIFReadWriter >> #readBitDataOnFrame:.

I am separated out from GIFReadWriter for clarity and better debugging.



See:
https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch
https://www.w3.org/Graphics/GIF/spec-gif89a.txt


"
Class {
	#name : #LzwGifDecoder,
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
	#category : #'Graphics-Files'
}

{ #category : #private }
LzwGifDecoder >> checkCodeSize [
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

{ #category : #private }
LzwGifDecoder >> codeStream: aReadableStream [ 
	"Set the stream of encoded bytes we will decode
	to be the internal codeStream. We read off the first
	byte immediately, which tells us how many subsequent bytes
	to use in our buffer for decoding"
	| chunkSize |
	codeStream := aReadableStream.
	chunkSize := codeStream next.
	codeStreamBuffer := ReadStream on: (codeStream next: chunkSize).
]

{ #category : #api }
LzwGifDecoder >> decode [
	| currentCode prevCode outStream |
	self initializeTables.
	outStream := WriteStream with: (ByteArray new).
	numLeftoverBits := 0.
	currentCode := self nextCode.
	currentCode = clearCode ifFalse: [ ^ self error: 'First code on the stream should always be the clear code!' ].
	
	"The first real code simply gets output
	onto the stream, then we enter the loop"
	currentCode := self nextCode.
	self writeBit: currentCode on: outStream.
	prevCode := currentCode.
	currentCode := self nextCode.
	[ currentCode = eoiCode ] whileFalse: [ 
		currentCode = clearCode
			ifTrue: [ 
				self initializeTables.
				currentCode := self nextCode.
				self
					writeBit: (suffixTable at: (currentCode + 1))
					on: outStream.
				prevCode := nil ]
			ifFalse: [ self handleCode: currentCode withPreviousCode: prevCode on: outStream ].
		prevCode := currentCode.
		currentCode := self nextCode ].
	^ outStream contents.

	
	
]

{ #category : #private }
LzwGifDecoder >> handleCode: anInteger withPreviousCode: prevInteger on: aWriteStream [
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
					self writeBit: first on: aWriteStream ]
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
						self writeBit: int on: aWriteStream ] ]. 
			]
		ifFalse: [ 
			"Here, the incoming code is not yet in the code tables"
			prevInteger < clearCode
				ifTrue: [ 
					first := (suffixTable at: (prevInteger + 1)).
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
						self writeBit: int on: aWriteStream ].
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
LzwGifDecoder >> initializeTables [
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

{ #category : #accessing }
LzwGifDecoder >> maxCode: anInteger [ 
	maxCode := anInteger
]

{ #category : #accessing }
LzwGifDecoder >> minimumCodeSize: anInteger [ 
	minimumCodeSize := anInteger
]

{ #category : #'private - packing' }
LzwGifDecoder >> nextByte [
	| nextChunkSize |
	codeStreamBuffer atEnd
		ifTrue: [ 
			nextChunkSize := codeStream next.
			nextChunkSize = 0 ifTrue: [ ^ self error: 'Next chunk size was 0!' ].
			codeStreamBuffer := (codeStream next: nextChunkSize) readStream ].
	^ codeStreamBuffer next.
]

{ #category : #private }
LzwGifDecoder >> nextCode [
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

{ #category : #accessing }
LzwGifDecoder >> onDecodedBit: aBlock [
	"This block will be executed once each time a new
	value is decoded from the stream, with the value
	as the sole argument passed to the block"
	outBlock := aBlock
]

{ #category : #'private - packing' }
LzwGifDecoder >> peekByte [
	| nextChunkSize |
	codeStreamBuffer atEnd
		ifTrue: [ 
			nextChunkSize := codeStream next.
			nextChunkSize = 0 ifTrue: [ ^ self error: 'Next chunk size is 0!' ].
			codeStreamBuffer := (codeStream next: nextChunkSize) readStream ].
	^ codeStreamBuffer peek.
]

{ #category : #writing }
LzwGifDecoder >> writeBit: anInteger on: aWriteStream [
	"Write the incoming decoded value onto a
	writestream. If I have an outBlock set, 
	send this value also"
	aWriteStream nextPut: anInteger.
	outBlock ifNil: [ ^ self ].
	outBlock value: anInteger.
]
