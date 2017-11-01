"
I am a concrete subclass of AbstractBinaryFileStream for stdio streams. I cannot modify the position of the underlying file.

Warning: Do not use me! You can access stdio streams through Stdio interface:

Stdio stdin.
Stdio stdout.
Stdio stderr.
"
Class {
	#name : #StdioStream,
	#superclass : #AbstractBinaryFileStream,
	#instVars : [
		'peekBuffer'
	],
	#category : 'Files-Streams'
}

{ #category : #testing }
StdioStream >> atEnd [

	^ file atEnd
]

{ #category : #accessing }
StdioStream >> next: n [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read startingAt |
	readBuffer := ByteArray new: n.
	startingAt := 1.
	peekBuffer ifNotNil: [ 
		readBuffer at: 1 put: peekBuffer.
		startingAt := startingAt + 1.
		peekBuffer := nil ].
	read := self primRead: handle into: readBuffer startingAt: startingAt count: n - startingAt + 1.
	^read = (n - startingAt + 1)
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : #accessing }
StdioStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self atEnd ifTrue: [^ nil ].

	peekBuffer ifNotNil: [ ^ peekBuffer ].

	^ peekBuffer := self next.
]

{ #category : #positioning }
StdioStream >> position [

	^ file position
]
