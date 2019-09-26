"
I am ZnBufferedReadWriteStream.
I wrap a buffered read stream and a buffered write stream on the same file.

I discard my read buffer on writes, and flush my write buffer on reads.
Make sure to always send me #flush or #close when you're done,
otherwise the last buffer might not yet have been written.
My class side's #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBufferedReadWriteStream,
	#superclass : #Object,
	#instVars : [
		'innerStream',
		'buffer',
		'bufferLength',
		'streamSize',
		'bufferOffset',
		'nextPosition',
		'isDirty'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnBufferedReadWriteStream class >> on: writeStream [
	^ self basicNew
		on: writeStream;
		yourself
]

{ #category : #convenience }
ZnBufferedReadWriteStream class >> on: readStream do: block [
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	| stream |

	stream := self on: readStream.

	^ [block value: stream ] ensure: [ stream flush ]
]

{ #category : #testing }
ZnBufferedReadWriteStream >> atEnd [
	
	^ self atEnd: nextPosition
]

{ #category : #private }
ZnBufferedReadWriteStream >> atEnd: anInteger [ 
	
	anInteger < streamSize ifTrue: [ ^ false ].
	anInteger <= (bufferOffset + bufferLength)  ifTrue: [ ^ false ].
	
	^ true
]

{ #category : #private }
ZnBufferedReadWriteStream >> bufferAt: aPosition [

	^ buffer at:  (aPosition - bufferOffset)
	
	
]

{ #category : #private }
ZnBufferedReadWriteStream >> bufferAt: aPosition put: anElement [

	self checkBufferFor: nextPosition.

	bufferLength := (aPosition - bufferOffset) max: bufferLength.
	buffer at: (aPosition - bufferOffset) put: anElement
	
]

{ #category : #private }
ZnBufferedReadWriteStream >> checkBufferFor: aPosition [

	(self isPositionInBuffer: aPosition)
		ifFalse: [ self refreshBufferFrom: aPosition ]
]

{ #category : #closing }
ZnBufferedReadWriteStream >> close [
	
	self flush.
	innerStream close
]

{ #category : #testing }
ZnBufferedReadWriteStream >> closed [

	^ innerStream closed
]

{ #category : #'initialize-release' }
ZnBufferedReadWriteStream >> collectionSpecies [
	^ innerStream isBinary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]
]

{ #category : #initialization }
ZnBufferedReadWriteStream >> defaultBufferSize [
	
	^ 2 raisedToInteger: 16
]

{ #category : #writing }
ZnBufferedReadWriteStream >> flush [
	
	isDirty ifFalse: [ ^ self ]. 
		
	innerStream position: bufferOffset.
	innerStream next: bufferLength putAll: buffer startingAt: 1.
	
	innerStream flush.
	
	streamSize := innerStream size.
	
	isDirty := false
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isBinary [
	
	^ innerStream isBinary 
]

{ #category : #private }
ZnBufferedReadWriteStream >> isPositionInBuffer: aPosition [

	^ aPosition between: bufferOffset and: bufferOffset + bufferLength
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isReadOnly [
	
	^ false
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isStream [

	^ true
]

{ #category : #reading }
ZnBufferedReadWriteStream >> next [ 
	| value |

	self atEnd 
		ifTrue: [^ nil].	
		
	self checkBufferFor:nextPosition.
		
	value := self bufferAt: nextPosition.
	
	nextPosition := nextPosition + 1.

	^ value
]

{ #category : #reading }
ZnBufferedReadWriteStream >> next: aQuantity [

	| read collection |

	collection := self collectionSpecies new: aQuantity.	

	read := self 
		readInto: collection 
		startingAt: 1 
		count: aQuantity.

	^ read = aQuantity 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: read - 1 ]     
]

{ #category : #writing }
ZnBufferedReadWriteStream >> next: aQuantity putAll: aCollection startingAt: startingAt [

	aCollection readStreamDo: [ :s | 
		s skip: startingAt - 1.
		self nextPutAll:  (s next: aQuantity)].
]

{ #category : #writing }
ZnBufferedReadWriteStream >> nextPut: anElement [ 
	
	self checkBufferFor: nextPosition.
		
	self bufferAt: nextPosition put: anElement.
	
	isDirty := true.
	nextPosition := nextPosition + 1
]

{ #category : #writing }
ZnBufferedReadWriteStream >> nextPutAll: aCollection [
	
	aCollection do: [ :each | self nextPut: each ]
]

{ #category : #'instance creation' }
ZnBufferedReadWriteStream >> on: aStream [

	innerStream := aStream.
	nextPosition := aStream position + 1.
	streamSize := aStream size.
	
	bufferOffset := -1.
	bufferLength := 0.
	isDirty := false.
	
	self sizeBuffer: self defaultBufferSize.
	

]

{ #category : #reading }
ZnBufferedReadWriteStream >> peek [
	
	| value |
	value := self next.
	"If I have read correctly I reset the position"
	value ifNotNil: [ nextPosition := nextPosition - 1  ].
	
	^ value
	
]

{ #category : #querying }
ZnBufferedReadWriteStream >> position [

	^ nextPosition - 1 

]

{ #category : #querying }
ZnBufferedReadWriteStream >> position: aNewPosition [

	^ nextPosition := aNewPosition + 1

]

{ #category : #reading }
ZnBufferedReadWriteStream >> readInto: aBuffer startingAt: startingAt count: count [

	| remainingCount maxPositionInBuffer read countToRead |

	remainingCount := count.
	read := 0.
	
	[ remainingCount > 0  and: [ self atEnd not ]]
		whileTrue: [  
			self checkBufferFor: nextPosition.
			
			maxPositionInBuffer := bufferOffset + bufferLength.
			countToRead := ( maxPositionInBuffer - (nextPosition - 1) ) min: remainingCount. 
			aBuffer
				replaceFrom: startingAt + read
				to: startingAt + read + countToRead - 1
				with: buffer
				startingAt: (nextPosition - bufferOffset).
				
			nextPosition := nextPosition + countToRead.
			remainingCount := remainingCount - countToRead.
			read := read + countToRead
		].
	
	^ read.
]

{ #category : #private }
ZnBufferedReadWriteStream >> refreshBufferFrom: aPosition [

	| nextBufferPosition |

	nextBufferPosition := (((aPosition - 1) max:0) // buffer size) * buffer size.
	bufferOffset = nextBufferPosition ifTrue: [ ^ self ].

	self flush.

	"If the position is outside the real stream I will only flush the buffer if I have to empty it."
	(nextBufferPosition >= streamSize) 
		ifTrue: [
			bufferOffset := nextBufferPosition.
			bufferLength := 0.	
			^ self	].
		 
	(nextBufferPosition = (bufferOffset + bufferLength)) 
		ifFalse: [ innerStream position: nextBufferPosition ].

	bufferLength := innerStream readInto: buffer startingAt: 1 count: buffer size.
	bufferOffset := nextBufferPosition.
]

{ #category : #reading }
ZnBufferedReadWriteStream >> setToEnd [

	nextPosition := (streamSize max: (bufferOffset + bufferLength)) + 1
]

{ #category : #querying }
ZnBufferedReadWriteStream >> size [ 

	^ streamSize max: (bufferOffset + bufferLength)
]

{ #category : #'initialize-release' }
ZnBufferedReadWriteStream >> sizeBuffer: size [

	bufferLength > 0 ifTrue: [ self flush ].
	bufferLength := 0.

	buffer := self collectionSpecies new: size
]

{ #category : #reading }
ZnBufferedReadWriteStream >> skip: aQuantity [

	nextPosition := nextPosition + aQuantity
]

{ #category : #reading }
ZnBufferedReadWriteStream >> upTo: value [ 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream.
	This could be further optimzed."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
]

{ #category : #reading }
ZnBufferedReadWriteStream >> upToEnd [
	"Read elements until the stream is atEnd and return them as a collection."

	| toRead |

	toRead := (streamSize - (nextPosition - 1)) max: 0.
	^ self next: toRead.
]
