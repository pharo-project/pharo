"
I am a read stream that reads and returns chunks of code. That is, pieces of text separated by

!!

symbols
"
Class {
	#name : 'ChunkReadStream',
	#superclass : 'DecoratorStream',
	#category : 'System-Sources-Streams',
	#package : 'System-Sources',
	#tag : 'Streams'
}

{ #category : 'decorated' }
ChunkReadStream >> next [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| out ch |
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := decoratedStream next) isNil ] whileFalse: [
			ch == self terminatorMark ifTrue: [
					decoratedStream peek == self terminatorMark
						ifTrue: [ decoratedStream next "skip doubled terminator" ]
						ifFalse: [ ^ out contents "terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents
]

{ #category : 'accessing' }
ChunkReadStream >> position: anInteger [

	decoratedStream position: anInteger
]

{ #category : 'initialization' }
ChunkReadStream >> reset [
	decoratedStream reset
]

{ #category : 'public' }
ChunkReadStream >> skipSeparators [

	[self atEnd not and: [ self peek isSeparator ]]
		whileTrue: [ decoratedStream next ]
]

{ #category : 'public' }
ChunkReadStream >> terminatorMark [
	^$!
]
