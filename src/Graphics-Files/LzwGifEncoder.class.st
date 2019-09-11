"
I implement the modified Lempel-Ziv-Welch (LZW) algorithm for lossless GIF bitmap compression. My primary purpose is to encode/compress streams of bitmap bytes as specified by the GIF standard.

My instances require at minimum:
- A size of bytes in a row of bitmap bits for the image (#rowByteSize:)
- The extent of the image being encoded (#extent:)
- An array of bits in a bitmap (as bytes) for encoding (sent with #encode:)
- A stream of Bytes on which to output the encoded bytes (#codeStream:)
- A minimum code size as specified from GIF header information (#minimimCodeSize:)

Once all of these are set, implementors simply send the #encode: message along with a
collection of bitmap values as bytes that need to be encoded. Instead of responding with a collection of encoded bytes, #encode: will write to the output stream specified by #codeStream: directly.

For an example of use, see GIFReadWriter >> #writeBitData:

NOTE: LZW compression for GIFs is complex and the #encode: method is largely taken verbatim from Kazuki Yasumatsu's 1995 Squeak implementation (as opposed to the Decoder, which was heavily refactored for readability and comprehension). Any contributions to fleshing this out in a comprehensible way are much appreciated!

See:
https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch
https://www.w3.org/Graphics/GIF/spec-gif89a.txt
"
Class {
	#name : #LzwGifEncoder,
	#superclass : #Object,
	#instVars : [
		'suffixTable',
		'prefixTable',
		'eoiCode',
		'clearCode',
		'codeSize',
		'minimumCodeSize',
		'maxCode',
		'nextAvailableCode',
		'numLeftoverBits',
		'bitBuffer',
		'codeStream',
		'codeStreamBuffer',
		'rowByteSize',
		'xPos',
		'yPos',
		'dimensions'
	],
	#category : #'Graphics-Files'
}

{ #category : #private }
LzwGifEncoder >> checkCodeSize [
	"Determine whether or not we need to increment
	the codeSize"
	(nextAvailableCode > maxCode and: [ codeSize < 12 ])
		ifTrue: [ 
			codeSize := codeSize + 1.
			maxCode := (1 bitShift: codeSize) - 1 ].
]

{ #category : #private }
LzwGifEncoder >> checkSettings [
	"Ensure that the appropriate variables
	that are needed for proper encoding
	have been set"
	codeStream ifNil: [ ^ self error: 'You must set a codeStream (byte stream) to write onto!' ].
	dimensions ifNil: [ ^ self error: 'You must provide the extent of the image we will encode!' ].
	rowByteSize ifNil: [ ^ self error: 'You must provide a rowByteSize for the supplied image bits!' ].
]

{ #category : #accessing }
LzwGifEncoder >> codeStream: aByteStream [
	codeStream := aByteStream.
]

{ #category : #accessing }
LzwGifEncoder >> dimensions: anExtentPoint [
	"Set the extent (as point) of the
	image that will be encoded"
	dimensions := anExtentPoint
]

{ #category : #converting }
LzwGifEncoder >> encode: bits [
	| maxBits maxMaxCode tSize tShift fCode ent pixel index nomatch disp |
	self checkSettings.
	xPos := yPos := 0.
	codeStream nextPut: minimumCodeSize.
	bitBuffer := 0.
	numLeftoverBits := 0.
	codeStreamBuffer := WriteStream on: (ByteArray new: 256).
	self initializeParameters.
	
	"These temp vars are taken from the
	original GIFReadWriter implementation"
	maxBits := 12.
	maxMaxCode := 1 bitShift: maxBits.
	tSize := 5003.
	prefixTable := Array new: tSize.
	suffixTable := Array new: tSize.
	tShift := 0.
	fCode := tSize.
	[ fCode < 65536 ] whileTrue: [ 
		tShift := tShift + 1.
		fCode := fCode * 2 ].
	tShift := 8 - tShift.
	1 to: tSize do: [ :i |
		suffixTable at: i put: -1 ].
	
	"We immediately write the clearCode
	to the output stream"
	self writeCodeAndCheckCodeSize: clearCode.
	
	"This loop is also taken from the original
	GIFReadWriter implementation"
	ent := self readPixelFrom: bits.
	[ (pixel := self readPixelFrom: bits) == nil ] whileFalse: 
		[ fCode := (pixel bitShift: maxBits) + ent.
		index := ((pixel bitShift: tShift) bitXor: ent) + 1.
		(suffixTable at: index) = fCode 
			ifTrue: [ ent := prefixTable at: index ]
			ifFalse: 
				[ nomatch := true.
				(suffixTable at: index) >= 0 ifTrue: 
					[ disp := tSize - index + 1.
					index = 1 ifTrue: [ disp := 1 ].
					"probe"
					
					[ (index := index - disp) < 1 ifTrue: [ index := index + tSize ].
					(suffixTable at: index) = fCode ifTrue: 
						[ ent := prefixTable at: index.
						nomatch := false
						"continue whileFalse:" ].
					nomatch and: [ (suffixTable at: index) > 0 ] ] whileTrue: 
						[ "probe"
						 ] ].
				"nomatch"
				nomatch ifTrue: 
					[ self writeCodeAndCheckCodeSize: ent.
					ent := pixel.
					nextAvailableCode < maxMaxCode 
						ifTrue: 
							[ prefixTable 
								at: index
								put: nextAvailableCode.
							suffixTable 
								at: index
								put: fCode.
							nextAvailableCode := nextAvailableCode + 1 ]
						ifFalse: 
							[ self writeCodeAndCheckCodeSize: clearCode.
							1 
								to: tSize
								do: 
									[ :i | 
									suffixTable 
										at: i
										put: -1 ].
							self initializeParameters ] ] ] ].
	prefixTable := suffixTable := nil.
	self writeCodeAndCheckCodeSize: ent.
	self writeCodeAndCheckCodeSize: eoiCode.
	self flushBits.
	codeStream nextPut: 0.
				
]

{ #category : #accessing }
LzwGifEncoder >> extent: anExtentPoint [
	"Set the extent (as point) of the
	image that will be encoded"
	dimensions := anExtentPoint
]

{ #category : #'private-bits access' }
LzwGifEncoder >> flushBits [
	numLeftoverBits = 0 ifFalse: 
		[ self nextBytePut: bitBuffer.
		numLeftoverBits := 0 ].
	self flushBuffer
]

{ #category : #private }
LzwGifEncoder >> flushBuffer [
	"Write out the current codeStreamBuffer size,
	followed by its actual contents, to the true
	output codeStream"
	codeStreamBuffer isEmpty ifTrue: [ ^ self ].
	codeStream
		nextPut: codeStreamBuffer size;
		nextPutAll: codeStreamBuffer contents.
	codeStreamBuffer := (ByteArray new: 256) writeStream.
]

{ #category : #initialization }
LzwGifEncoder >> initializeParameters [
	"The initial code size and mask settings
	also get reinitialized each time"
	codeSize := minimumCodeSize + 1.
	clearCode := (1 bitShift: minimumCodeSize).
	eoiCode := clearCode + 1.
	nextAvailableCode := clearCode + 2.
	maxCode := (1 bitShift: codeSize) - 1.
]

{ #category : #accessing }
LzwGifEncoder >> minimumCodeSize: anInteger [
	minimumCodeSize := anInteger
]

{ #category : #'private-packing' }
LzwGifEncoder >> nextBytePut: anInteger [
	"Write a complete byte to the output byteStream.
	Be sure to reset one we reach the limit, which is
	255 for GIF files. Then write the length of the next
	byte chunks to the stream also"
	codeStreamBuffer nextPut: anInteger.
	codeStreamBuffer size >= 254
		ifTrue: [ self flushBuffer ].
]

{ #category : #'private-bits access' }
LzwGifEncoder >> nextCodePut: anInteger [
	"Attempt to put the bits on the
	output stream. If we have remaining bits,
	then we need to use bitwise operations to
	fill the next byte properly before putting
	it on the output stream"
	| numBitsWritten shiftCount newInteger |
	shiftCount := 0.
	numLeftoverBits = 0
		ifTrue: [ 
			numBitsWritten := 8.
			newInteger := anInteger ]
		ifFalse: [ 
			numBitsWritten := numLeftoverBits.
			newInteger := bitBuffer + (anInteger bitShift: 8 - numLeftoverBits) ].
	[ numBitsWritten < codeSize ] whileTrue: [ 
		self nextBytePut: ((newInteger bitShift: shiftCount) bitAnd: 255).
		shiftCount := shiftCount - 8.
		numBitsWritten := numBitsWritten + 8 ].
	(numLeftoverBits := numBitsWritten - codeSize) = 0
		ifTrue: [ self nextBytePut: (newInteger bitShift: shiftCount) ]
		ifFalse: [ bitBuffer := newInteger bitShift: shiftCount ].
	^ anInteger
	
]

{ #category : #'private-encoding' }
LzwGifEncoder >> readPixelFrom: bits [
	"Using the current x and y positions and
	the specified byte size for a row, determine
	the value for the next pixel in the provided bits"
	| pixel |
	yPos >= (dimensions y) ifTrue: [ ^ nil ].
	pixel := bits byteAt: yPos * rowByteSize + xPos + 1.
	self updatePixelPosition.
	^ pixel
	
]

{ #category : #accessing }
LzwGifEncoder >> rowByteSize: anInteger [
	rowByteSize := anInteger
]

{ #category : #private }
LzwGifEncoder >> updatePixelPosition [
	"Increment the xPos. If we are at the width
	position, set xPos to 0 and increment the yPos"
	xPos := xPos + 1.
	xPos >= (dimensions x) ifFalse: [ ^ self ].
	xPos := 0.
	yPos := yPos + 1.
]

{ #category : #writing }
LzwGifEncoder >> writeCodeAndCheckCodeSize: aCode [
	self nextCodePut: aCode.
	self checkCodeSize.
]
