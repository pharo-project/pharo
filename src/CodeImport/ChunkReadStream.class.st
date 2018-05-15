"
I am a read stream that reads and returns chunks of code. That is, pieces of text separated by

!!

symbols
"
Class {
	#name : #ChunkReadStream,
	#superclass : #DecoratorStream,
	#instVars : [
		'nextChar'
	],
	#category : #'CodeImport-Utilities'
}

{ #category : #decorated }
ChunkReadStream >> basicNext [

	nextChar = self terminatorMark 
		ifTrue: [ decoratedStream next ].
	^ nextChar
]

{ #category : #decorated }
ChunkReadStream >> basicNextChunk [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| out ch |
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := decoratedStream next) isNil ]
		whileFalse: [ 
			ch == self terminatorMark
				ifTrue: [ 
					decoratedStream peek == self terminatorMark
						ifTrue: [ decoratedStream next	"skip doubled terminator" ]
						ifFalse: [ ^ out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents
]

{ #category : #decorated }
ChunkReadStream >> decodeString: string andRuns: runsRaw [
	| strm runLength runValues newString index |
	strm := ReadStream on: runsRaw from: 1 to: runsRaw size.
	(strm peekFor: $()
		ifFalse: [ ^ nil ].
	runLength := OrderedCollection new.
	[ 
	strm skipSeparators.
	strm peekFor: $) ] whileFalse: [ runLength add: (Number readFrom: strm) ].
	runValues := OrderedCollection new.
	[ strm atEnd ]
		whileFalse: [ 
			runValues add: (Number readFrom: strm).
			strm next ].
	newString := WideString new: string size.
	index := 1.
	runLength
		with: runValues
		do: [ :length :leadingChar | 
			index to: index + length - 1 do: [ :pos | newString at: pos put: (Character leadingChar: leadingChar code: (string at: pos) charCode) ].
			index := index + length ].
	^ newString
]

{ #category : #testing }
ChunkReadStream >> isNextChunkMetaData [
	| isMetaData |
	isMetaData := decoratedStream peek = $!.
	isMetaData ifTrue: [ decoratedStream next ].
	^ isMetaData
]

{ #category : #testing }
ChunkReadStream >> isNextStyleChunk [
	"Style chunks are between $] and $["
	^ decoratedStream peek == $]
]

{ #category : #decorated }
ChunkReadStream >> next [

    ^ self parseLangTagFor: self basicNextChunk
]

{ #category : #decorated }
ChunkReadStream >> parseLangTagFor: aString [

	| string peek runsRaw foundTag |
	string := aString.
	"Test for ]lang[ tag"
	peek := self skipSeparators; peek.
	peek = $] ifFalse: [ ^ string ].  "no tag"
	foundTag := (decoratedStream upTo: $[).
	foundTag = ']lang' ifTrue: [
		runsRaw := self basicNextChunk.
		string := self decodeString: aString andRuns: runsRaw
	] ifFalse: [
		"If we find a ] but not a lang tag, there is something really wrong and we are not ready to parse that"
		^ string , self basicNextChunk
	].
	^ string
]

{ #category : #decorated }
ChunkReadStream >> position [
	
	^ decoratedStream position - (nextChar ifNil: [0] ifNotNil: [1])
]

{ #category : #public }
ChunkReadStream >> readUpToEndOfStyleChunk [
	"Style chunks are between $] and $["
	^decoratedStream upTo: $[
]

{ #category : #decorated }
ChunkReadStream >> reset [
	nextChar := nil.
	decoratedStream reset.
]

{ #category : #public }
ChunkReadStream >> skipSeparators [

	[self atEnd not and: [ self peek isSeparator ]]
		whileTrue: [ decoratedStream next ]
]

{ #category : #public }
ChunkReadStream >> terminatorMark [
	^$!
]
