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

{ #category : #'initialize-release' }
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

{ #category : #accessing }
SourceFile >> flush [

	stream flush
]

{ #category : #accessing }
SourceFile >> fullName [

	^ path asString
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

{ #category : #'fileIn/Out' }
SourceFile >> nextChunk [

	^ (ChunkReadStream on: stream) next
]

{ #category : #'fileIn/Out' }
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
SourceFile >> path: arg1 [
	path := arg1
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
				[ stream := ZnCharacterReadWriteStream
					on: (each asFileReference / basename) binaryReadWriteStream
					encoding: 'utf8'.
				^ self ] on: Error do: [  ] ] ].

	potentialLocations do: [ :each | 
			[ stream := (each asFileReference / basename) readStream.
			^ self ] on: Error do: [  ] ]
]

{ #category : #accessing }
SourceFile >> upToEnd [
	
	^ stream upToEnd
]
