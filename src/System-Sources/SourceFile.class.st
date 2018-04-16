"
asdasd
"
Class {
	#name : #SourceFile,
	#superclass : #Object,
	#instVars : [
		'path',
		'stream',
		'potentialLocations',
		'reference'
	],
	#category : #'System-Sources'
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

{ #category : #stream }
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

{ #category : #stream }
SourceFile >> cr [

	stream cr
]

{ #category : #stream }
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

{ #category : #testing }
SourceFile >> isResolved [

	^ reference notNil
]

{ #category : #actions }
SourceFile >> moveTo: aDestination [

	self isResolved ifFalse: [ self error: 'Cannot move a non resolved source file' ].
	reference moveTo: aDestination
]

{ #category : #'instance creation' }
SourceFile >> newLine [

	stream cr
]

{ #category : #stream }
SourceFile >> next [

	^ stream next
]

{ #category : #stream }
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

{ #category : #stream }
SourceFile >> nextPut: aCharacter [

	stream nextPut: aCharacter
]

{ #category : #stream }
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

{ #category : #stream }
SourceFile >> position [

	^ stream position
]

{ #category : #stream }
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

{ #category : #stream }
SourceFile >> setToEnd [

	stream setToEnd
]

{ #category : #stream }
SourceFile >> size [

	^ stream size
]

{ #category : #stream }
SourceFile >> skip: anInteger [

	stream skip: anInteger
]

{ #category : #stream }
SourceFile >> space [

	stream space
]

{ #category : #accessing }
SourceFile >> stream: aStream [

	stream := aStream
]

{ #category : #stream }
SourceFile >> tryOpen [

	self tryOpenReadOnly: false
]

{ #category : #stream }
SourceFile >> tryOpenReadOnly: readOnly [
	| basename potentialReference |
	basename := path asFileReference basename.

	"Open a read write stream only if read only access was not requested.
	We need to create the encoding and buffering streams manually because we need a read write stream."
	readOnly
		ifFalse: [ potentialLocations
				do: [ :each | 
					[ potentialReference := each asFileReference / basename.
					stream := ZnCharacterReadWriteStream on: (ZnBufferedReadWriteStream on: potentialReference binaryWriteStream) encoding: 'utf8'.
					reference := potentialReference.
					^ self ]
						on: Error
						do: [  ] ] ].
	potentialLocations
		do: [ :each | 
			[ potentialReference := each asFileReference / basename.
			stream := potentialReference readStream.
			reference := potentialReference.
			^ self ]
				on: Error
				do: [  ] ]
]

{ #category : #accessing }
SourceFile >> upToEnd [
	
	^ stream upToEnd
]
