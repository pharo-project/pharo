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
ChunkWriteStream >> bang [
	
	decoratedStream nextPut: self terminatorMark
]

{ #category : #'reflective operations' }
ChunkWriteStream >> doesNotUnderstand: aMessage [
	"Forward message to decoree"

	^ aMessage sendTo: decoratedStream
]

{ #category : #accessing }
ChunkWriteStream >> nextChunkPut: aString [

	^ self nextPut: aString
]

{ #category : #accessing }
ChunkWriteStream >> nextPut: aString [
	"Append the argument, aString, to the receiver, doubling embedded ! terminators and adding a extra one"

	| string start bangIndex |
	string := aString asString.
	start := 1.
	[ (bangIndex := string indexOf: self terminatorMark startingAt: start) = 0 ]
		whileFalse: [
			decoratedStream next: bangIndex - start + 1 putAll: string startingAt: start.
			self bang. "double it"
			start := bangIndex + 1 ].
	decoratedStream next: string size - start + 1 putAll: string startingAt: start.
	self bang. "one extra"
	decoratedStream flush

]

{ #category : #accessing }
ChunkWriteStream >> terminatorMark [
	^$!
]
