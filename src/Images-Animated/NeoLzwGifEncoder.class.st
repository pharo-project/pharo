Class {
	#name : #NeoLzwGifEncoder,
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
		'bitBuffer',
		'bitStream',
		'codeStream',
		'codeStreamBuffer',
		'outBlock',
		'rowByteSize',
		'xPos',
		'yPos',
		'dimensions'
	],
	#category : #'Images-Animated'
}

{ #category : #private }
NeoLzwGifEncoder >> checkCodeSize [
	"Determine whether or not we need to increment
	the codeSize"
	(nextAvailableCode > maxCode and: [ codeSize < 12 ])
		ifTrue: [ 
			codeSize := codeSize + 1.
			maxCode := (1 bitShift: codeSize) - 1 ].
]

{ #category : #'as yet unclassified' }
NeoLzwGifEncoder >> checkSettings [
	"Ensure that the appropriate variables
	that are needed for proper encoding
	have been set"
	codeStream ifNil: [ ^ self error: 'You must set a codeStream (byte stream) to write onto!' ].
	dimensions ifNil: [ ^ self error: 'You must provide the extent of the image we will encode!' ].
	rowByteSize ifNil: [ ^ self error: 'You must provide a rowByteSize for the supplied image bits!' ].
]

{ #category : #accessing }
NeoLzwGifEncoder >> codeStream: aByteStream [
	codeStream := aByteStream.
]

{ #category : #accessing }
NeoLzwGifEncoder >> dimensions: anExtentPoint [
	"Set the extent (as point) of the
	image that will be encoded"
	dimensions := anExtentPoint
]

{ #category : #converting }
NeoLzwGifEncoder >> encode: bits [
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
NeoLzwGifEncoder >> extent: anExtentPoint [
	"Set the extent (as point) of the
	image that will be encoded"
	dimensions := anExtentPoint
]

{ #category : #'private-bits access' }
NeoLzwGifEncoder >> flushBits [
	numLeftoverBits = 0 ifFalse: 
		[ self nextBytePut: bitBuffer.
		numLeftoverBits := 0 ].
	self flushBuffer
]

{ #category : #private }
NeoLzwGifEncoder >> flushBuffer [
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
NeoLzwGifEncoder >> initializeParameters [
	"The initial code size and mask settings
	also get reinitialized each time"
	codeSize := minimumCodeSize + 1.
	clearCode := (1 bitShift: minimumCodeSize).
	eoiCode := clearCode + 1.
	nextAvailableCode := clearCode + 2.
	maxCode := (1 bitShift: codeSize) - 1.
]

{ #category : #accessing }
NeoLzwGifEncoder >> minimumCodeSize: anInteger [
	minimumCodeSize := anInteger
]

{ #category : #'private-packing' }
NeoLzwGifEncoder >> nextBytePut: anInteger [
	"Write a complete byte to the output byteStream.
	Be sure to reset one we reach the limit, which is
	255 for GIF files. Then write the length of the next
	byte chunks to the stream also"
	codeStreamBuffer nextPut: anInteger.
	codeStreamBuffer size >= 254
		ifTrue: [ self flushBuffer ].
]

{ #category : #'private-bits access' }
NeoLzwGifEncoder >> nextCodePut: anInteger [
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
NeoLzwGifEncoder >> readPixelFrom: bits [
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
NeoLzwGifEncoder >> rowByteSize: anInteger [
	rowByteSize := anInteger
]

{ #category : #'as yet unclassified' }
NeoLzwGifEncoder >> tablesInclude: aValue withIndexBuffer: aCollection [
	"Answer true if we are able to find the given value
	in the prefix/suffix tables by tracing the values
	in the given collection. False otherwise."
	| lookupIndex |
	aCollection ifEmpty: [ 
		^ prefixTable includes: aValue ].
	aCollection do: [ :val |
		 ]
	
]

{ #category : #private }
NeoLzwGifEncoder >> updatePixelPosition [
	"Increment the xPos. If we are at the width
	position, set xPos to 0 and increment the yPos"
	xPos := xPos + 1.
	xPos >= (dimensions x) ifFalse: [ ^ self ].
	xPos := 0.
	yPos := yPos + 1.
]

{ #category : #writing }
NeoLzwGifEncoder >> writeCodeAndCheckCodeSize: aCode [
	self nextCodePut: aCode.
	self checkCodeSize.
]
