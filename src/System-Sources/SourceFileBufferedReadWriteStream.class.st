"
I am Buffered Read Write Stream.
I should only be used in the SourceFile implementation
"
Class {
	#name : #SourceFileBufferedReadWriteStream,
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
	#category : #'System-Sources-Streams'
}

{ #category : #'instance creation' }
SourceFileBufferedReadWriteStream class >> on: writeStream [
	^ self basicNew
		on: writeStream;
		yourself
]

{ #category : #convenience }
SourceFileBufferedReadWriteStream class >> on: readStream do: block [
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	| stream |

	stream := self on: readStream.

	^ [block value: stream ] ensure: [ stream flush ]
]

{ #category : #testing }
SourceFileBufferedReadWriteStream >> atEnd [
	
	^ self atEnd: nextPosition
]

{ #category : #private }
SourceFileBufferedReadWriteStream >> atEnd: anInteger [ 
	
	anInteger < streamSize ifTrue: [ ^ false ].
	anInteger <= (bufferOffset + bufferLength)  ifTrue: [ ^ false ].
	
	^ true
]

{ #category : #private }
SourceFileBufferedReadWriteStream >> bufferAt: aPosition [

	^ buffer at:  (aPosition - bufferOffset)
	
	
]

{ #category : #private }
SourceFileBufferedReadWriteStream >> bufferAt: aPosition put: anElement [

	self checkBufferFor: nextPosition.

	bufferLength := (aPosition - bufferOffset) max: bufferLength.
	buffer at: (aPosition - bufferOffset) put: anElement
	
]

{ #category : #private }
SourceFileBufferedReadWriteStream >> checkBufferFor: aPosition [

	(self isPositionInBuffer: aPosition)
		ifFalse: [ self refreshBufferFrom: aPosition ]
]

{ #category : #closing }
SourceFileBufferedReadWriteStream >> close [
	
	self flush.
	innerStream close
]

{ #category : #testing }
SourceFileBufferedReadWriteStream >> closed [

	^ innerStream closed
]

{ #category : #'initialize-release' }
SourceFileBufferedReadWriteStream >> collectionSpecies [
	^ innerStream isBinary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]
]

{ #category : #initialization }
SourceFileBufferedReadWriteStream >> defaultBufferSize [
	
	^ 2 raisedToInteger: 16
]

{ #category : #writing }
SourceFileBufferedReadWriteStream >> ensureWrittenPosition: aPosition [
	isDirty ifFalse: [ ^ false ].

	((self isPositionInBuffer: aPosition) or: [
		"The position has been written to disk but
		the buffer is dirty, so there is potentially
		data in the buffer that we need."
		aPosition <= bufferOffset ]) ifTrue: [
			self flush.
			^ true ].

	^ false
]

{ #category : #writing }
SourceFileBufferedReadWriteStream >> flush [
	
	isDirty ifFalse: [ ^ self ]. 
		
	innerStream position: bufferOffset.
	innerStream next: bufferLength putAll: buffer startingAt: 1.
	
	innerStream flush.
	
	streamSize := innerStream size.
	
	isDirty := false
]

{ #category : #testing }
SourceFileBufferedReadWriteStream >> isBinary [
	
	^ innerStream isBinary 
]

{ #category : #private }
SourceFileBufferedReadWriteStream >> isPositionInBuffer: aPosition [

	^ aPosition between: bufferOffset and: bufferOffset + bufferLength
]

{ #category : #testing }
SourceFileBufferedReadWriteStream >> isReadOnly [
	
	^ false
]

{ #category : #testing }
SourceFileBufferedReadWriteStream >> isStream [

	^ true
]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> next [ 
	| value |

	self atEnd 
		ifTrue: [^ nil].	
		
	self checkBufferFor:nextPosition.
		
	value := self bufferAt: nextPosition.
	
	nextPosition := nextPosition + 1.

	^ value
]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> next: aQuantity [

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
SourceFileBufferedReadWriteStream >> next: aQuantity putAll: aCollection startingAt: startingAt [

	aCollection readStreamDo: [ :s | 
		s skip: startingAt - 1.
		self nextPutAll:  (s next: aQuantity)].
]

{ #category : #writing }
SourceFileBufferedReadWriteStream >> nextPut: anElement [ 
	
	self checkBufferFor: nextPosition.
		
	self bufferAt: nextPosition put: anElement.
	
	isDirty := true.
	nextPosition := nextPosition + 1
]

{ #category : #writing }
SourceFileBufferedReadWriteStream >> nextPutAll: aCollection [
	
	aCollection do: [ :each | self nextPut: each ]
]

{ #category : #'instance creation' }
SourceFileBufferedReadWriteStream >> on: aStream [

	innerStream := aStream.
	nextPosition := aStream position + 1.
	streamSize := aStream size.
	
	bufferOffset := -1.
	bufferLength := 0.
	isDirty := false.
	
	self sizeBuffer: self defaultBufferSize.
	

]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> peek [
	
	| value |
	value := self next.
	"If I have read correctly I reset the position"
	value ifNotNil: [ nextPosition := nextPosition - 1  ].
	
	^ value
	
]

{ #category : #querying }
SourceFileBufferedReadWriteStream >> position [

	^ nextPosition - 1 

]

{ #category : #querying }
SourceFileBufferedReadWriteStream >> position: aNewPosition [

	^ nextPosition := aNewPosition + 1

]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> readInto: aBuffer startingAt: startingAt count: count [

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
SourceFileBufferedReadWriteStream >> refreshBufferFrom: aPosition [

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
SourceFileBufferedReadWriteStream >> setToEnd [

	nextPosition := (streamSize max: (bufferOffset + bufferLength)) + 1
]

{ #category : #querying }
SourceFileBufferedReadWriteStream >> size [ 

	^ streamSize max: (bufferOffset + bufferLength)
]

{ #category : #'initialize-release' }
SourceFileBufferedReadWriteStream >> sizeBuffer: size [

	bufferLength > 0 ifTrue: [ self flush ].
	bufferLength := 0.

	buffer := self collectionSpecies new: size
]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> skip: aQuantity [

	nextPosition := nextPosition + aQuantity
]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> upTo: value [ 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream.
	This could be further optimzed."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
]

{ #category : #reading }
SourceFileBufferedReadWriteStream >> upToEnd [
	"Read elements until the stream is atEnd and return them as a collection."

	| toRead |

	toRead := (streamSize - (nextPosition - 1)) max: 0.
	^ self next: toRead.
]
