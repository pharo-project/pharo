"
asdasd
"
Class {
	#name : #SourceFile,
	#superclass : #Object,
	#instVars : [
		'path',
		'stream',
		'potentialLocations'
	],
	#category : #'System-Sources-Sources'
}

{ #category : #'instance creation' }
SourceFile class >> on: aPath potentialLocations: locations [

	^ self new
		path: aPath;
		potentialLocations: locations;
		yourself
]

{ #category : #testing }
SourceFile >> atEnd [
	
	^ stream atEnd
]

{ #category : #initialization }
SourceFile >> close [

	stream ifNil: [ ^ self ].
	stream closed ifTrue: [ 
		stream := nil.
		^ self ].
	stream close.
	stream := nil.
]

{ #category : #accessing }
SourceFile >> closed [
	^ stream isNil or: [ stream closed ]
]

{ #category : #'stop conditions' }
SourceFile >> cr [

	stream cr
]

{ #category : #buffering }
SourceFile >> discardBuffer [

	stream wrappedStream discardBuffer
]

{ #category : #accessing }
SourceFile >> ensureWrittenPosition: aPosition [

	self isReadOnly ifTrue: [ ^ false ].
	
	^ stream ensureWrittenPosition: aPosition
]

{ #category : #accessing }
SourceFile >> flush [

	(stream isNil or: [ stream isReadOnly ])
		ifTrue: [ ^ self ].
	
	stream flush
]

{ #category : #accessing }
SourceFile >> fullName [

	^ path asString
]

{ #category : #accessing }
SourceFile >> getPreambleAt: startingPosition [
	"Search backwards from byte startingPosition in my stream for a method preamble and return it.
	A method preamble looks like: MyClass methodsFor: 'test' stamp: 'author 1/27/2019 12:27'
	but with exclamation marks ($!) around it (the contents excluding the $!'s is returned).
	startingPosition should be set one position before the closing $!"
	
	| characterReadStream binaryStream encoder position |
	"I hold either a ZnCharacterReadStream or a ZnCharacterReadWriteStream (see #tryOpenReadOnly:)
	Use #isReadOnly and #readOnlyCopy to access the ZnCharacterReadStream in both cases"
	characterReadStream := self isReadOnly ifTrue: [ stream ] ifFalse: [ stream readOnlyCopy ].
	"Access the binary read stream wrapped by the character read stream"
	binaryStream := characterReadStream wrappedStream.
	"Access the encoder held by the character read stream"
	encoder := characterReadStream encoder.
	"Search backwards for the previous occurrence of $!
	Although the underlying encoding is UTF-8 we can still operate/move at the byte level
	since $! code 33 cannot occur in code points encoded using 2, 3 or 4 bytes"
	position := startingPosition.
	[ position >= 0 
		and: [ 
			binaryStream position: position. 
			binaryStream next ~= 33 "$!" ] ] 
		whileTrue: [ position := position - 1 ].
	"Now that we found the byte range, extract and decode it"
	^ encoder decodeBytes: (binaryStream next: startingPosition - position)
]

{ #category : #testing }
SourceFile >> isOpen [

	^ stream notNil and: [ stream closed not ]
]

{ #category : #testing }
SourceFile >> isReadOnly [

	^ stream isReadOnly
]

{ #category : #accessing }
SourceFile >> next [

	^ stream next
]

{ #category : #accessing }
SourceFile >> next: anInteger [

	^ stream next: anInteger
]

{ #category : #accessing }
SourceFile >> next: anInteger putAll: aString startingAt: startIndex [

	stream next: anInteger putAll: aString startingAt: startIndex

]

{ #category : #'file in/out' }
SourceFile >> nextChunk [

	^ (ChunkReadStream on: stream) next
]

{ #category : #'file in/out' }
SourceFile >> nextChunkPut: aChunk [

	^ (ChunkWriteStream on: stream) nextPut: aChunk
]

{ #category : #accessing }
SourceFile >> nextPut: aCharacter [

	stream nextPut: aCharacter
]

{ #category : #accessing }
SourceFile >> nextPutAll: aString [

	stream nextPutAll: aString
]

{ #category : #accessing }
SourceFile >> path [
	^ path
]

{ #category : #accessing }
SourceFile >> path: aPath [
	path := aPath
]

{ #category : #accessing }
SourceFile >> peek [
	
	^ stream peek
]

{ #category : #accessing }
SourceFile >> position [

	^ stream position
]

{ #category : #accessing }
SourceFile >> position: anInteger [

	stream position: anInteger
]

{ #category : #accessing }
SourceFile >> potentialLocations [

	^ potentialLocations
]

{ #category : #accessing }
SourceFile >> potentialLocations: aCollection [

	potentialLocations := aCollection
]

{ #category : #printing }
SourceFile >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self fullName;
		nextPut: $)
]

{ #category : #accessing }
SourceFile >> readOnlyCopy [

	^ (self species on: path potentialLocations: potentialLocations)
		tryOpenReadOnly: true;
		yourself
]

{ #category : #initialization }
SourceFile >> setToEnd [

	stream setToEnd
]

{ #category : #accessing }
SourceFile >> size [

	^ stream size
]

{ #category : #accessing }
SourceFile >> skip: anInteger [

	stream skip: anInteger
]

{ #category : #streaming }
SourceFile >> space [

	stream space
]

{ #category : #accessing }
SourceFile >> stream: aStream [

	stream := aStream
]

{ #category : #opening }
SourceFile >> tryOpen [

	self tryOpenReadOnly: false
]

{ #category : #opening }
SourceFile >> tryOpenReadOnly: readOnly [
	| basename |
	basename := path asFileReference basename.

	"Open a read write stream only if read only access was not requested.
	We need to create the encoding and buffering streams manually because we need a read write stream."
	readOnly ifFalse: [
		potentialLocations
			do: [ :each | 
				[ stream := SourceFileCharacterReadWriteStream
					on: (SourceFileBufferedReadWriteStream on:(each asFileReference / basename) unbufferedBinaryWriteStream)
					encoding: 'utf8'.
				^ self ] on: Error do: [  ] ] ].

	potentialLocations do: [ :each | 
			[ stream := ZnCharacterReadStream
					on: (each asFileReference / basename) binaryReadStream
					encoding: 'utf8'.
				^ self ] on: Error do: [  ] ]
]

{ #category : #accessing }
SourceFile >> upToEnd [
	
	^ stream upToEnd
]
