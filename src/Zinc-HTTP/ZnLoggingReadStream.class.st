"
I am ZnLoggingReadStream, I wrap a binary stream and log all read operations.

At logLevel 1 (the default) I log read operations next and friends.
At logLevel 2 I also log peek and atEnd.

Usage

ZnServer default reader: [ :stream | 
	ZnRequest readFrom: 
		((ZnLoggingReadStream on: stream) identifier: 'Zn request') ].

ZnServer default reader: [ :stream | ZnRequest readFrom: stream ].

ZnZincServerAdaptor default server reader: [ :stream | 
	ZnRequest readBinaryFrom: 
		((ZnLoggingReadStream on: stream) identifier: 'Zn request') ].

ZnZincServerAdaptor default server reader: [ :stream | ZnRequest readBinaryFrom: stream ].

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnLoggingReadStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'logger',
		'identifier',
		'logLevel'
	],
	#category : #'Zinc-HTTP-Streaming'
}

{ #category : #'instance creation' }
ZnLoggingReadStream class >> on: binaryReadStream [
	^ self new
		on: binaryReadStream;
		yourself
]

{ #category : #testing }
ZnLoggingReadStream >> atEnd [
	| boolean |
	boolean := stream atEnd.
	logLevel > 1
		ifTrue: [
			logger value: ('{1} atEnd: {2}' format: { identifier. boolean }) ].
	^ boolean
]

{ #category : #accessing }
ZnLoggingReadStream >> collectionSpecies [
	^ ByteArray
]

{ #category : #'initialize-release' }
ZnLoggingReadStream >> identifier: string [
	identifier := string
]

{ #category : #initialization }
ZnLoggingReadStream >> initialize [
	super initialize.
	logger := [ :string | Transcript crShow: string ].
	identifier := self class name asString.
	logLevel := 1
]

{ #category : #testing }
ZnLoggingReadStream >> isBinary [
	^ true
]

{ #category : #'initialize-release' }
ZnLoggingReadStream >> logLevel: integer [
	"logLevel 1 logs reads with next and friends
	logLevl 2 logs peek and atEnd too"

	logLevel := integer
]

{ #category : #'initialize-release' }
ZnLoggingReadStream >> logger: oneArgumentBlock [
	logger := oneArgumentBlock
]

{ #category : #accessing }
ZnLoggingReadStream >> next [
	| byte |
	byte := stream next.
	logger value: ('{1} read 1 byte: {2}' format: {
		identifier.
		byte ifNotNil: [ byte printStringBase: 16 length: 2 padded: true ] }).
	^ byte
]

{ #category : #accessing }
ZnLoggingReadStream >> next: requestedCount [
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self
		next: requestedCount
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
ZnLoggingReadStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"

	^ self
		next: requestedCount
		into: collection
		startingAt: 1
]

{ #category : #accessing }
ZnLoggingReadStream >> next: requestedCount into: collection startingAt: offset [
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"

	| readCount |
	readCount := self
		readInto: collection
		startingAt: offset
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]
]

{ #category : #'initialize-release' }
ZnLoggingReadStream >> on: binaryReadStream [
	stream := binaryReadStream
]

{ #category : #accessing }
ZnLoggingReadStream >> peek [
	| byte |
	byte := stream peek.
	logLevel > 1
		ifTrue: [
			logger value: ('{1} peeked 1 byte: {2}' format: {
				identifier.
				byte printStringBase: 16 length: 2 padded: true }) ].
	^ byte
]

{ #category : #accessing }
ZnLoggingReadStream >> readInto: collection startingAt: offset count: count [
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."

	| bytesRead message |
	bytesRead := stream readInto: collection startingAt: offset count: count.
	message := bytesRead isZero
		ifTrue: [ '{1} read 0 bytes' format: { identifier } ]
		ifFalse: [
			String streamContents: [ :out |
				out nextPutAll: identifier; space.
				out nextPutAll: 'read '; print: bytesRead;
				nextPutAll: (bytesRead = 1 ifTrue: [ ' byte: ' ] ifFalse: [ ' bytes: ']).
			offset to: offset + bytesRead - 1 do: [ :index |
				(collection at: index) printOn: out base: 16 length: 2 padded: true ] ] ].
	logger value: message.
	^ bytesRead
]

{ #category : #accessing }
ZnLoggingReadStream >> upToEnd [
	^ ZnUtils readUpToEnd: self limit: nil
]

{ #category : #accessing }
ZnLoggingReadStream >> wrappedStream [
	^ stream
]
