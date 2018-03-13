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

{ #category : #'as yet unclassified' }
SourceFile >> close [

	stream ifNil: [ ^ self ].
	
	stream close.
	stream := nil.
]

{ #category : #accessing }
SourceFile >> closed [
	^ stream isNil or: [ stream closed ]
]

{ #category : #'as yet unclassified' }
SourceFile >> cr [

	stream cr
]

{ #category : #'as yet unclassified' }
SourceFile >> flush [

	stream flush
]

{ #category : #'as yet unclassified' }
SourceFile >> fullName [

	^ path asString
]

{ #category : #'as yet unclassified' }
SourceFile >> isOpen [

	^ stream notNil and: [ stream closed not ]
]

{ #category : #'as yet unclassified' }
SourceFile >> isReadOnly [

	^ stream isReadOnly
]

{ #category : #'as yet unclassified' }
SourceFile >> next [

	^ stream next
]

{ #category : #'as yet unclassified' }
SourceFile >> next: anInteger [

	^ stream next: anInteger
]

{ #category : #'fileIn/Out' }
SourceFile >> nextChunk [

	^ (ChunkReadStream on: stream) next
]

{ #category : #'as yet unclassified' }
SourceFile >> nextPut: aCharacter [

	stream nextPut: aCharacter
]

{ #category : #'as yet unclassified' }
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

{ #category : #'as yet unclassified' }
SourceFile >> position [

	^ stream position
]

{ #category : #'as yet unclassified' }
SourceFile >> position: anInteger [

	stream position: anInteger
]

{ #category : #'as yet unclassified' }
SourceFile >> potentialLocations [

	^ potentialLocations
]

{ #category : #'as yet unclassified' }
SourceFile >> potentialLocations: aCollection [

	potentialLocations := aCollection
]

{ #category : #'as yet unclassified' }
SourceFile >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self fullName;
		nextPut: $)
]

{ #category : #'as yet unclassified' }
SourceFile >> readOnlyCopy [

	^ (self species on: path potentialLocations: potentialLocations)
		tryOpenReadOnly: true;
		yourself
]

{ #category : #'as yet unclassified' }
SourceFile >> setToEnd [

	stream setToEnd
]

{ #category : #'as yet unclassified' }
SourceFile >> size [

	^ stream size
]

{ #category : #'as yet unclassified' }
SourceFile >> skip: anInteger [

	stream skip: anInteger
]

{ #category : #'as yet unclassified' }
SourceFile >> space [

	stream space
]

{ #category : #accessing }
SourceFile >> stream: aStream [

	stream := aStream
]

{ #category : #'as yet unclassified' }
SourceFile >> tryOpen [

	self tryOpenReadOnly: false
]

{ #category : #'as yet unclassified' }
SourceFile >> tryOpenReadOnly: readOnly [
	| basename |
	basename := path asFileReference basename.

	"Open a read write stream only if read only access was not requested"
	readOnly ifFalse: [
		potentialLocations
			do: [ :each | 
				[ stream := ZnCharacterReadWriteStream
					on: (File named: (each asFileReference / basename) fullName) writeStream
					encoding: 'utf8'.
				^ self ] on: Error do: [  ] ] ].

	potentialLocations do: [ :each | 
			[ stream := ZnCharacterReadStream
					on: (File named: (each asFileReference / basename) fullName) readStream
					encoding: 'utf8'.
			^ self ] on: Error do: [  ] ]
]

{ #category : #accessing }
SourceFile >> upToEnd [
	
	^ stream upToEnd
]
