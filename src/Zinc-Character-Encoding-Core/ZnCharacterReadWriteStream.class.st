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
ZnCharacterReadWriteStream >> close [
	
	writeStream close
]

{ #category : #testing }
ZnCharacterReadWriteStream >> closed [
	^ writeStream closed
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
ZnCharacterReadWriteStream >> position [

	^ readStream position
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
