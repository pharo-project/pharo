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
		'readStream',
		'writeStream',
		'lastRead'
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

	^ block value: (self on: readStream)
]

{ #category : #testing }
ZnBufferedReadWriteStream >> atEnd [
	
	^ self readingActionDo: [ readStream atEnd ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> close [
	
	writeStream flush.
	writeStream close.
]

{ #category : #testing }
ZnBufferedReadWriteStream >> closed [
	^ readStream closed
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> flush [
	
	self writingActionDo: [ writeStream flush ]
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isBinary [
	
	^ readStream isBinary
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isReadOnly [
	
	^ false
]

{ #category : #testing }
ZnBufferedReadWriteStream >> isStream [

	^ true
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> next [
	
	^ self readingActionDo: [ 
		readStream next ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> next: anInteger [ 
	
	^ self readingActionDo: [ 
		readStream next: anInteger ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> next: count putAll: collection [

	self writingActionDo: [ 
		writeStream next: count putAll: collection ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> next: count putAll: collection startingAt: offset [
	
	self writingActionDo: [
		writeStream next: count putAll: collection startingAt: offset ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> nextPut: aCharacter [ 
	
	self writingActionDo: [ writeStream nextPut: aCharacter ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> nextPutAll: aString [ 
	
	^ self writingActionDo: [ writeStream nextPutAll: aString ]
]

{ #category : #'instance creation' }
ZnBufferedReadWriteStream >> on: aStream [

	lastRead := true.
	readStream := ZnBufferedReadStream on: aStream.
	writeStream := ZnBufferedWriteStream on: aStream.
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> peek [
	
	^ self readingActionDo: [ 
		readStream peek ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> position [
	
	^ lastRead
		ifTrue: [ readStream position ]
		ifFalse: [ writeStream position ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> position: anInteger [ 
	
	self writingActionDo: [ 
		writeStream position: anInteger ]
	
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> readInto: collection startingAt: offset count: requestedCount [
	
	^ self readingActionDo: [ 
		readStream readInto: collection startingAt: offset count: requestedCount ]
]

{ #category : #private }
ZnBufferedReadWriteStream >> readingActionDo: aBlock [

	"Reading from the read stream.
	We should 
	 - flush the write stream
	 - discard the read buffer (which may contain incorrect data).
	 - and then perform the read."
	
	lastRead ifFalse: [ 
		writeStream flush.
		readStream discardBuffer ].
	^ aBlock ensure: [ lastRead := true ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> setToEnd [
	
	^ self writingActionDo: [ 
		writeStream setToEnd ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> size [
	^ readStream size
]

{ #category : #'initialize-release' }
ZnBufferedReadWriteStream >> sizeBuffer: anInteger [ 
	
	readStream sizeBuffer: anInteger.
	writeStream sizeBuffer: anInteger.
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> skip: anInteger [ 
	
	self readingActionDo: [ 
		readStream skip: anInteger ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> truncate [
	
	self writingActionDo: [ writeStream truncate ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> truncate: anInteger [ 
	
	self writingActionDo: [ writeStream truncate: anInteger ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> upTo: aCharacter [ 
	
	^ self readingActionDo: [ readStream upTo: aCharacter ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> upToEnd [
	
	^ self readingActionDo: [ readStream upToEnd ]
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> wrappedStream [

	^ readStream wrappedStream
]

{ #category : #accessing }
ZnBufferedReadWriteStream >> writingActionDo: aBlock [
	
	"Writing to the write stream.
	We should 
	 - write the write stream
	 - discard the read buffer (which may contain incorrect data)"
	lastRead ifTrue: [ 
		writeStream discardBuffer ].
	readStream discardBuffer.
	^ aBlock ensure: [ lastRead := false ]
]
