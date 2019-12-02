"
I am a special ChunkWriteStream used to write on the streams without the flushing part.
If the flushing part can be changed in my super class then my existence must be terminated.
"
Class {
	#name : #SourceChunkWriteStream,
	#superclass : #ChunkWriteStream,
	#category : #'System-Sources-Utilities'
}

{ #category : #accessing }
SourceChunkWriteStream >> nextPut: aString [
	"Append the argument, aString, to the receiver, doubling embedded ! terminators and adding a extra one"

	| string start bangIndex |
	string := aString asString.
	start := 1.
	[ 
	(bangIndex := string indexOf: self terminatorMark startingAt: start)
	= 0 ] whileFalse: [ 
		decoratedStream
			next: bangIndex - start + 1
			putAll: string
			startingAt: start.
		self bang. "double it"
		start := bangIndex + 1 ].
	decoratedStream
		next: string size - start + 1
		putAll: string
		startingAt: start.
	self bang. "one extra"
	decoratedStream setToEnd
]
