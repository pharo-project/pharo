"
I am a write stream wrapping a second stream. Whenever they ask me to write a cr, a lf, or a crlf I'll instead print a portable new line depending on the platform I'm on.

stream := '' writeStream.
converter := ZnCrPortableWriteStream on: stream.
converter cr; cr; lf; nextPut: $a.
stream contents
"
Class {
	#name : #ZnCrPortableWriteStream,
	#superclass : #WriteStream,
	#instVars : [
		'stream',
		'previous'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnCrPortableWriteStream class >> on: aStream [

	^ self basicNew
		stream: aStream;
		yourself
]

{ #category : #accessing }
ZnCrPortableWriteStream >> newLine [
	previous := nil.
	stream nextPutAll: OSPlatform current lineEnding
]

{ #category : #accessing }
ZnCrPortableWriteStream >> nextPut: aCharacter [

	(String crlf includes: aCharacter )
		ifFalse: [ 
			previous ifNotNil: [ self newLine ].
			^ stream nextPut: aCharacter ].

	previous = Character cr
		ifTrue: [ self newLine ].
	aCharacter ~= Character lf
		ifTrue: [ previous := aCharacter ].
]

{ #category : #accessing }
ZnCrPortableWriteStream >> stream: aWriteStream [ 
	stream := aWriteStream
]
