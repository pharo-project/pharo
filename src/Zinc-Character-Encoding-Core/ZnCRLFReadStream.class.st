"
ZnCRLFReadStream wraps a binary stream with any of the common line endings (CR, LF, CRLF) and converts them to CRLF.

RFC 2045 (https://tools.ietf.org/html/rfc2045) states that MIME documents use CRLF as the line end marker, however email documents as stored on disk often use the local line enging, e.g. LF.


Public API and Key Messages

- on: - supply the stream to be wrapped
- The public API is the standard Stream API

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	next:			<Integer>
	stream:		<BinaryStream>


    Implementation Points
"
Class {
	#name : #ZnCRLFReadStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'next'
	],
	#classVars : [
		'Cr',
		'Lf'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'class initialization' }
ZnCRLFReadStream class >> initialize [

	Cr := Character cr asInteger.
	Lf := Character lf asInteger
]

{ #category : #'instance creation' }
ZnCRLFReadStream class >> on: aBinaryReadStream [

	^self new on: aBinaryReadStream
]

{ #category : #accessing }
ZnCRLFReadStream >> atEnd [

	^stream atEnd
]

{ #category : #'open/close' }
ZnCRLFReadStream >> close [

	stream close
]

{ #category : #accessing }
ZnCRLFReadStream >> isBinary [

	^true
]

{ #category : #accessing }
ZnCRLFReadStream >> next [
	"Answer the next character from the stream, converting end-of-lines to CRLF"

	| byte |

	next ifNotNil:
		[ byte := next.
		next := nil.
		^byte ].
	stream atEnd ifTrue: [ ^nil ].
	(byte := stream next) ifNil: [ ^nil ].
	byte == Cr ifTrue:
		"Consume the Cr and ensure that a Lf is answered next.
		If the following character is Lf, consume it."
		[ stream peek == Lf ifTrue:
			[ stream next ].
		next := Lf ]
	ifFalse: [ byte == Lf ifTrue:
		[ "Answer a Cr instead, and then a Lf"
		byte := Cr.
		next := Lf ] ].
	^byte
]

{ #category : #accessing }
ZnCRLFReadStream >> next: n into: aCollection [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1
]

{ #category : #accessing }
ZnCRLFReadStream >> next: requestedCount into: aCollection startingAt: startIndex [
	"Read requestedCount objects into the given collection.
	Return aCollection or a partial copy if less elements have been read."

	| readCount |
	readCount := self readInto: aCollection startingAt: startIndex count: requestedCount.
	^ readCount = requestedCount
		ifTrue: [ aCollection ]
		ifFalse: [ aCollection copyFrom: 1 to: startIndex + readCount - 1 ]
]

{ #category : #accessing }
ZnCRLFReadStream >> on: aBinaryReadStream [

	self assert: aBinaryReadStream isBinary.
	stream := aBinaryReadStream
]

{ #category : #accessing }
ZnCRLFReadStream >> peek [
	"Answer the next character from the stream, converting end-of-lines to CRLF"

	| byte |

	next ifNotNil:	[ ^next ].
	stream atEnd ifTrue: [ ^nil ].
	(byte := stream peek) ifNil: [ ^nil ].
	byte == Lf ifTrue: [ ^Cr ].
	^byte
]

{ #category : #accessing }
ZnCRLFReadStream >> readInto: collection startingAt: offset count: requestedCount [

	0 to: requestedCount - 1 do: [ :count | | byte |
		(byte := self next) ifNil: [ ^ count ].
		collection at: offset + count put: byte ].
	^ requestedCount
]

{ #category : #accessing }
ZnCRLFReadStream >> readStream [
	^ self
]

{ #category : #accessing }
ZnCRLFReadStream >> upToEnd [
	"Answer a ByteArray of the stream from the current position to the last"

	^ByteArray streamContents: [ :newStream | | nextByte |
		[ (nextByte := self next) isNil ] whileFalse:
			[ newStream nextPut: nextByte ] ]
]
