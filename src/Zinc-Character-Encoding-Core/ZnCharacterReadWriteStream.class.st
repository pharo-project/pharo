"
I am a read-write character stream. I am mainly used to open the Pharo source and changes files.
"
Class {
	#name : #ZnCharacterReadWriteStream,
	#superclass : #Object,
	#instVars : [
		'readStream',
		'writeStream'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnCharacterReadWriteStream class >> on: wrappedStream encoding: encoding [

	^ self new
		on: wrappedStream encoding: encoding;
		yourself
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> atEnd [
	
	^ readStream atEnd
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> close [
	
	writeStream close
]

{ #category : #testing }
ZnCharacterReadWriteStream >> closed [
	^ writeStream closed
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> collectionSpecies [
	^ String
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> cr [
	
	writeStream cr
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> flush [

	writeStream flush
]

{ #category : #testing }
ZnCharacterReadWriteStream >> isReadOnly [
	
	^ false
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> next [
	
	^ readStream next
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> next: anInteger [ 
	
	^ readStream next: anInteger
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> nextPut: aCharacter [

	^ writeStream nextPut: aCharacter
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> nextPutAll: aString [

	^ writeStream nextPutAll: aString
]

{ #category : #'instance creation' }
ZnCharacterReadWriteStream >> on: aStream encoding: encoding [
	| encoder |
	encoder := encoding asZnCharacterEncoder.
	readStream := ZnCharacterReadStream on: aStream encoding: encoder.
	writeStream := ZnCharacterWriteStream on: aStream encoding: encoder
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> peek [
	
	^ readStream peek
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> position [

	^ readStream position
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> position: anInteger [ 
	
	readStream position: anInteger
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> readOnlyCopy [

	^ readStream
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> setToEnd [

	writeStream setToEnd
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> size [

	^ readStream size
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> skip: anInteger [ 
	
	readStream skip: anInteger
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> space [
	
	writeStream space
]

{ #category : #accessing }
ZnCharacterReadWriteStream >> upToAll: aCollection [
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
ZnCharacterReadWriteStream >> upToEnd [
	
	^ readStream upToEnd
]
