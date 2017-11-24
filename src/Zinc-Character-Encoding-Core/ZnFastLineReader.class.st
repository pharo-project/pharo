"
I am ZnFastLineReader, a helper to efficiently read CR, LF or CRLF terminated lines from a character stream.
"
Class {
	#name : #ZnFastLineReader,
	#superclass : #Object,
	#instVars : [
		'readStream',
		'cr',
		'lf',
		'bufferStream'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnFastLineReader class >> on: characterReadStream [
	^ self new 
		on: characterReadStream;
		yourself
]

{ #category : #testing }
ZnFastLineReader >> atEnd [
	^ readStream atEnd
]

{ #category : #initialize }
ZnFastLineReader >> beWide [
	self bufferStream: (WideString new: 32) writeStream
]

{ #category : #initialize }
ZnFastLineReader >> bufferStream: characterWriteStream [
	^ bufferStream := characterWriteStream
]

{ #category : #initialize }
ZnFastLineReader >> close [
	readStream close
]

{ #category : #initialize }
ZnFastLineReader >> initialize [
	super initialize.
	cr := Character cr.
	lf := Character lf
]

{ #category : #accessing }
ZnFastLineReader >> linesDo: block [
	[ self atEnd ]
		whileFalse: [ 
			| line |
			line := self nextLine.
			line isNil
				ifFalse: [ block value: line ] ]
]

{ #category : #accessing }
ZnFastLineReader >> nextLine [
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
	self atEnd ifTrue: [ ^ nil ].
	^ self streamContents: [ :out | | eol char |
		eol := false.
		[ eol ] whileFalse: [ 
			char := readStream next.
			(char isNil or: [ char == lf ])
				ifTrue: [ eol := true ]
				ifFalse: [ 
					char == cr 
						ifTrue: [ eol := true. readStream peekFor: lf ]
						ifFalse: [  out nextPut: char ] ] ] ]
]

{ #category : #initialize }
ZnFastLineReader >> on: characterReadStream [
	readStream := characterReadStream 
]

{ #category : #private }
ZnFastLineReader >> streamContents: block [
	"Like readStream collectionSpecies streamContents: block
	but reusing the underlying buffer for improved efficiency"
	
	bufferStream 
		ifNil: [ 
			bufferStream := (readStream collectionSpecies new: 32) writeStream ].
	bufferStream reset.
	block value: bufferStream.
	^ bufferStream contents
]
