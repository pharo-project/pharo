"
I am ZdcIOBuffer.

I manage a fixed SequenceableCollection buffer for simultaneous input and output.
I maintain a readPointer and a writePointer.

When data is written to me, it is stored at the end, past my writePointer.
When data is read from me, it is retrieved from the front, past my readPointer.

Invariant: readPointer <= writePointer

My valid contents for reading is defined from contentsStart to contentsEnd, from readPointer + 1 to writePointer.
Data can be added to the free space defined from freeSpaceStart to freeSpaceEnd, from writePointer + 1 to the buffer's' size.

There can be a gap at my front. Compacting moves data if necessary to make (more) room at the end.
"
Class {
	#name : #ZdcIOBuffer,
	#superclass : #Object,
	#instVars : [
		'buffer',
		'readPointer',
		'writePointer'
	],
	#category : #'Zodiac-Core'
}

{ #category : #'instance creation' }
ZdcIOBuffer class >> on: sequenceableCollection [
	^ (self new)
		on: sequenceableCollection;
		yourself
]

{ #category : #'instance creation' }
ZdcIOBuffer class >> onByteArrayOfSize: size [
	^ self on: (ZdcByteArrayManager current byteArrayOfSize: size zero: true)
]

{ #category : #accessing }
ZdcIOBuffer >> advanceReadPointer: count [
	"Advance the read pointer as if count elements were read"
	
	self assert: count >= 0.
	count <= self availableForReading
		ifTrue: [  
			readPointer := readPointer + count ]
		ifFalse: [
			self bufferEmptyError ]  
]

{ #category : #accessing }
ZdcIOBuffer >> advanceWritePointer: count [
	"Advance the write pointer as if count elements were written"
	
	self assert: count >= 0.
	count <= self availableForWriting 
		ifTrue: [  
			writePointer := writePointer + count ]
		ifFalse: [
			self bufferFullError ]  
]

{ #category : #accessing }
ZdcIOBuffer >> availableForReading [
	"How many elements can be read ?"

	^ writePointer - readPointer
]

{ #category : #accessing }
ZdcIOBuffer >> availableForWriting [
	"How many elements can be written at the end ?"
	
	^ buffer size - writePointer
]

{ #category : #accessing }
ZdcIOBuffer >> buffer [
	"Return our underlying buffer"
	
	^ buffer
]

{ #category : #private }
ZdcIOBuffer >> bufferEmptyError [
	^ self error: 'Buffer empty'
]

{ #category : #private }
ZdcIOBuffer >> bufferFullError [
	^ self error: 'Buffer full'
]

{ #category : #accessing }
ZdcIOBuffer >> bufferSize [
	"Return the raw size of the underlying buffer"
	
	buffer ifNil: [ ^ 0 ].
	^ buffer size
]

{ #category : #accessing }
ZdcIOBuffer >> capacity [
	"How much room is there both at the front and at the end ?"
	
	^ self gapAtFront + self availableForWriting 
]

{ #category : #'initialize-release' }
ZdcIOBuffer >> close [
	buffer ifNotNil: [  
		ZdcByteArrayManager current recycle: buffer.
		buffer := nil.
		self reset ]
]

{ #category : #operations }
ZdcIOBuffer >> compact [
	"Move data to the front of the buffer to make more room for writing at the end"
	
	| count |
	count := self availableForReading.
	count isZero ifTrue: [ ^ self reset ].
	readPointer isZero ifTrue: [ ^ self ].
	buffer replaceFrom: 1 to: count with: buffer startingAt: readPointer + 1.
	readPointer := 0.
	writePointer := count
]

{ #category : #operations }
ZdcIOBuffer >> contents [
	"Return our readable contents"
	
	^ self isEmpty
		ifTrue: [ buffer copyEmpty ]
		ifFalse: [ buffer copyFrom: self contentsStart to: self contentsEnd ]
]

{ #category : #accessing }
ZdcIOBuffer >> contentsEnd [
	"Return the current valid contents end index into the buffer (inclusive)"
	
	self isEmpty ifTrue: [ self bufferEmptyError ].
	^ writePointer
]

{ #category : #accessing }
ZdcIOBuffer >> contentsStart [
	"Return the current valid contents start index into the buffer (inclusive)"
	
	self isEmpty ifTrue: [ self bufferEmptyError ].
	^ readPointer + 1
]

{ #category : #accessing }
ZdcIOBuffer >> freeSpaceEnd [
	"Return the current valid free space end index into the buffer (inclusive)"
	
	self isFull ifTrue: [ self bufferFullError ]. 
	^ buffer size
]

{ #category : #accessing }
ZdcIOBuffer >> freeSpaceStart [
	"Return the current valid free space start index into the buffer (inclusive)"
	
	self isFull ifTrue: [ self bufferFullError ]. 
	^ writePointer + 1
]

{ #category : #accessing }
ZdcIOBuffer >> gapAtFront [
	"How many elements fit in the gap at the front ?"
	
	^ readPointer
]

{ #category : #testing }
ZdcIOBuffer >> isEmpty [
	"Can something be read ?"
	
	^ self availableForReading isZero
]

{ #category : #testing }
ZdcIOBuffer >> isFull [
	"Is there room for writing ?"
	
	^ self availableForWriting isZero
]

{ #category : #reading }
ZdcIOBuffer >> next [
	"Access the next readable element, fail is empty"

	^ self availableForReading > 0
		ifTrue: [
			readPointer := readPointer + 1.
			buffer at: readPointer ]
		ifFalse: [
			self bufferEmptyError ]
]

{ #category : #writing }
ZdcIOBuffer >> next: count putAll: collection startingAt: offset [
	"Add count elements from collection starting at offset. Fail if there is not enough room"
	
	| writeOffset |
	count > self availableForWriting ifTrue: [ self bufferFullError ].  
	writeOffset := self freeSpaceStart.
	buffer 
		replaceFrom: writeOffset 
		to: writeOffset + count - 1 
		with: collection 
		startingAt: offset.
	self advanceWritePointer: count
]

{ #category : #writing }
ZdcIOBuffer >> nextPut: anObject [
	"Add another element. Fail if full"
	
	self availableForWriting > 0
		ifTrue: [
			writePointer := writePointer + 1 .
			buffer at: writePointer put: anObject ]
		ifFalse: [
			self bufferFullError ].
	^ anObject

]

{ #category : #'initialize-release' }
ZdcIOBuffer >> on: sequenceableCollection [
	buffer := sequenceableCollection.
	self reset
]

{ #category : #reading }
ZdcIOBuffer >> peek [
	"Peek the next readable element, fail is empty"

	^ self availableForReading > 0
		ifTrue: [
			buffer at: readPointer + 1 ]
		ifFalse: [
			self bufferEmptyError ]
]

{ #category : #printing }
ZdcIOBuffer >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	buffer
		ifNil: [ stream << 'closed' ]
		ifNotNil: [  
			self gapAtFront > 0 
				ifTrue: [ stream nextPut: $-; print: self gapAtFront; space ].
			stream print: self contents.
			self availableForWriting > 0
				ifTrue: [ stream space; nextPut: $+; print: self availableForWriting ] ].
	stream nextPut: $)
]

{ #category : #reading }
ZdcIOBuffer >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."
	
	| toRead |
	toRead := requestedCount min: self availableForReading.
	collection 
		replaceFrom: offset 
		to: offset + toRead - 1 
		with: buffer 
		startingAt: self contentsStart.
	self advanceReadPointer: toRead.
	^ toRead
]

{ #category : #initialization }
ZdcIOBuffer >> reset [
	"Do a hard reset"
	
	readPointer := 0.
	writePointer := 0
]

{ #category : #accessing }
ZdcIOBuffer >> size [
	"Return the number of actual elements available"
	
	^ self availableForReading
]
