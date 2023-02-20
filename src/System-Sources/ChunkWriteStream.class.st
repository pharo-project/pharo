"
I am a write stream that write chunks of Strings. That is, pieces of text separated by

!!

symbols
"
Class {
	#name : #ChunkWriteStream,
	#superclass : #DecoratorStream,
	#category : #'System-Sources-Utilities'
}

{ #category : #writing }
ChunkWriteStream >> afterNexPut [

	decoratedStream flush
]

{ #category : #writing }
ChunkWriteStream >> bang [

	decoratedStream nextPut: self terminatorMark
]

{ #category : #'reflective operations' }
ChunkWriteStream >> doesNotUnderstand: aMessage [
	"Forward message to decoree"

	^ aMessage sendTo: decoratedStream
]

{ #category : #accessing }
ChunkWriteStream >> duplicateTerminatorMarkOn: aString [
	"Append the argument, aString, to the receiver, doubling embedded ! terminators and adding a extra one"

	| string start bangIndex newStringStream |
	string := aString asString.
	newStringStream := WriteStream on: (string species new: string size * 2).
	start := 1.

	[ (bangIndex := string indexOf: self terminatorMark startingAt: start) = 0 ]
		whileFalse: [
			newStringStream
				next: bangIndex - start + 1
				putAll: string
				startingAt: start.

			newStringStream nextPut: self terminatorMark. "double it"
			start := bangIndex + 1 ].

	newStringStream
		next: string size - start + 1
		putAll: string
		startingAt: start.

	newStringStream nextPut: self terminatorMark. "one extra"

	^ newStringStream contents
]

{ #category : #accessing }
ChunkWriteStream >> nextChunkPut: aString [

	^ self nextPut: aString
]

{ #category : #accessing }
ChunkWriteStream >> nextPut: aString [
	"Append the argument, aString, to the receiver, doubling embedded ! terminators and adding a extra one"

	| string |
	string := self duplicateTerminatorMarkOn: aString asString.

	decoratedStream
		next: string size
		putAll: string
		startingAt: 1.

	self afterNexPut
]

{ #category : #accessing }
ChunkWriteStream >> terminatorMark [
	^$!
]
