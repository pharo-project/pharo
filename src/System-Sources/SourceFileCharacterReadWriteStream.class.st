"
I am a read-write character stream. I am mainly used to open the Pharo source and changes files.
"
Class {
	#name : #SourceFileCharacterReadWriteStream,
	#superclass : #Object,
	#instVars : [
		'readStream',
		'writeStream'
	],
	#category : #'System-Sources-Streams'
}

{ #category : #'instance creation' }
SourceFileCharacterReadWriteStream class >> on: wrappedStream encoding: encoding [

	^ self new
		on: wrappedStream encoding: encoding;
		yourself
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> atEnd [
	
	^ readStream atEnd
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> close [
	
	writeStream close
]

{ #category : #testing }
SourceFileCharacterReadWriteStream >> closed [
	^ writeStream closed
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> collectionSpecies [
	^ String
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> cr [
	
	writeStream cr
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> ensureWrittenPosition: aPosition [
	^ writeStream wrappedStream ensureWrittenPosition: aPosition
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> flush [

	writeStream flush
]

{ #category : #testing }
SourceFileCharacterReadWriteStream >> isReadOnly [
	
	^ false
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> next [
	
	^ readStream next
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> next: anInteger [ 
	
	^ readStream next: anInteger
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> next: anInteger putAll: aString startingAt: startIndex [

	^ writeStream next: anInteger putAll: aString startingAt: startIndex
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> nextPut: aCharacter [

	^ writeStream nextPut: aCharacter
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> nextPutAll: aString [

	^ writeStream nextPutAll: aString
]

{ #category : #'instance creation' }
SourceFileCharacterReadWriteStream >> on: aStream encoding: encoding [
	| encoder |
	encoder := encoding asZnCharacterEncoder.
	readStream := ZnCharacterReadStream on: aStream encoding: encoder.
	writeStream := ZnCharacterWriteStream on: aStream encoding: encoder
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> peek [
	
	^ readStream peek
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> position [

	^ readStream position
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> position: anInteger [ 
	
	readStream position: anInteger
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> print: object [
	writeStream print: object
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> readOnlyCopy [

	^ readStream
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> setToEnd [

	writeStream setToEnd
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> size [

	^ readStream size
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> skip: anInteger [ 
	
	readStream skip: anInteger
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> space [
	
	writeStream space
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> tab [
	
	writeStream tab
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> upToAll: aCollection [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."
	
	^ self collectionSpecies streamContents: [ :out |
		| partialMatch pattern matched |
		partialMatch := (self collectionSpecies new: aCollection size) writeStream.
		pattern := aCollection readStream.
		matched := false.
		([ matched or: [ self atEnd or: [ pattern atEnd ] ] ]) whileFalse: [
			| ch |
			(ch := self next) = pattern next
				ifTrue: [
					pattern atEnd
						ifTrue: [ matched := true ]
						ifFalse: [ partialMatch nextPut: ch ] ]
				ifFalse: [
					pattern reset.
					out nextPutAll: partialMatch contents.
					partialMatch reset.
					out nextPut: ch ] ].
		matched ifFalse: [ out nextPutAll: partialMatch contents ] ]
]

{ #category : #accessing }
SourceFileCharacterReadWriteStream >> upToEnd [
	
	^ readStream upToEnd
]
