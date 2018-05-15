"
I am a write stream that write chunks of Strings. That is, pieces of text separated by

!!

symbols
"
Class {
	#name : #ChunkWriteStream,
	#superclass : #DecoratorStream,
	#category : #'CodeExport-Utilities'
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
    "Append the argument, aString, to the receiver, doubling embedded terminators."
 
    | i remainder terminator | 
    terminator := $!.
    remainder := aString asString.
    [(i := remainder indexOf: terminator) = 0] whileFalse:
        [decoratedStream nextPutAll: (remainder copyFrom: 1 to: i).
        decoratedStream nextPut: terminator.  "double imbedded terminators"
        remainder := remainder copyFrom: i+1 to: remainder size].
    decoratedStream nextPutAll: remainder. 
    decoratedStream nextPut: terminator.
    decoratedStream flush.
]

{ #category : #accessing }
ChunkWriteStream >> terminatorMark [
	^$!
]
