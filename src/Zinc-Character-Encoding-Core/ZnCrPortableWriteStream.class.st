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
	"Write aCharacter to the receivers stream.
	Convert all line end combinations, i.e cr, lf, crlf, to the platform convention"

	(aCharacter = Character lf and: [ previous = Character cr ]) ifFalse: [
		(String crlf includes: aCharacter) ifTrue: 
			[ self newLine ]
		ifFalse:
			[ stream nextPut: aCharacter ] ].
	previous := aCharacter.

]

{ #category : #accessing }
ZnCrPortableWriteStream >> stream: aWriteStream [ 
	stream := aWriteStream
]
