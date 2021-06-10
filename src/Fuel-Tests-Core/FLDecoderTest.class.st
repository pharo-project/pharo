Class {
	#name : #FLDecoderTest,
	#superclass : #TestCase,
	#instVars : [
		'stream',
		'decoder'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLDecoderTest >> setUp [
	super setUp.
	
	stream := ReadWriteStream on: ByteArray new.
	decoder := FLDecoder
		on: stream
		migrations: #()
		globalEnvironment: self class environment
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedInt24 [
	stream 
		nextPutAll: #[0 0 0];
		reset.
	self assert: decoder nextEncodedInt24 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 1];
		reset.
	self assert: decoder nextEncodedInt24 equals: 1.
	
	"-1 in two's complement"
	stream
		resetToStart;
		nextPutAll: #[255 255 255];
		reset.
	self assert: decoder nextEncodedInt24 equals: -1.
	
	"Maximum positive 24-bit integer in two's complement"
	stream
		resetToStart;
		nextPutAll: #[127 255 255];
		reset.
	self assert: decoder nextEncodedInt24 equals: (2 raisedTo: 23) - 1.
	
	"Minimum negative 24-bit integer in two's complement"
	stream
		resetToStart;
		nextPutAll: #[128 0 0 0];
		reset.
	self assert: decoder nextEncodedInt24 equals: 0 - (2 raisedTo: 23)
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedInt32 [
	stream 
		nextPutAll: #[0 0 0 0];
		reset.
	self assert: decoder nextEncodedInt32 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 0 1];
		reset.
	self assert: decoder nextEncodedInt32 equals: 1.
	
	"-1 on 32-bits systems"
	stream
		resetToStart;
		nextPutAll: #[127 255 255 255];
		reset.
	self assert: decoder nextEncodedInt32 equals: -1.
	
	"SmallInteger maxVal on 32-bits systems"
	stream
		resetToStart;
		nextPutAll: #[63 255 255 255];
		reset.
	self assert: decoder nextEncodedInt32 equals: (2 raisedTo: 30) - 1.
	
	"SmallInteger minVal on 32-bits systems"
	stream
		resetToStart;
		nextPutAll: #[64 0 0 0];
		reset.
	self assert: decoder nextEncodedInt32 equals: 0 - (2 raisedTo: 30)
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedInt64 [
	stream 
		nextPutAll: #[0 0 0 0 0 0 0 0];
		reset.
	self assert: decoder nextEncodedInt64 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 0 0 0 0 0 1];
		reset.
	self assert: decoder nextEncodedInt64 equals: 1.
	
	"-1 on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[31 255 255 255 255 255 255 255];
		reset.
	self assert: decoder nextEncodedInt64 equals: -1.
	
	"SmallInteger maxVal on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[15 255 255 255 255 255 255 255];
		reset.
	self assert: decoder nextEncodedInt64 equals: (2 raisedTo: 60) - 1.
	
	"SmallInteger minVal on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[16 0 0 0 0 0 0 0];
		reset.
	self assert: decoder nextEncodedInt64 equals: 0 - (2 raisedTo: 60)
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedUint16 [
	stream 
		nextPutAll: #[0 0];
		reset.
	self assert: decoder nextEncodedUint16 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 1];
		reset.
	self assert: decoder nextEncodedUint16 equals: 1.
	
	"Maximum positive 16-bit integer in two's complement"
	stream
		resetToStart;
		nextPutAll: #[255 255];
		reset.
	self assert: decoder nextEncodedUint16 equals: (2 raisedTo: 16) - 1
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedUint24 [
	stream 
		nextPutAll: #[0 0 0];
		reset.
	self assert: decoder nextEncodedUint24 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 1];
		reset.
	self assert: decoder nextEncodedUint24 equals: 1.
	
	"Maximum positive 24-bit integer in two's complement"
	stream
		resetToStart;
		nextPutAll: #[255 255 255];
		reset.
	self assert: decoder nextEncodedUint24 equals: (2 raisedTo: 24) - 1
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedUint32 [
	stream 
		nextPutAll: #[0 0 0 0];
		reset.
	self assert: decoder nextEncodedUint32 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 0 1];
		reset.
	self assert: decoder nextEncodedUint32 equals: 1.
	
	"SmallInteger maxVal on 32-bits systems"
	stream
		resetToStart;
		nextPutAll: #[127 255 255 255];
		reset.
	self assert: decoder nextEncodedUint32 equals: (2 raisedTo: 31) - 1.
	
	"SmallInteger minVal on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[64 0 0 0];
		reset.
	self assert: decoder nextEncodedUint32 equals: (2 raisedTo: 30)
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedUint64 [
	stream 
		nextPutAll: #[0 0 0 0 0 0 0 0];
		reset.
	self assert: decoder nextEncodedUint64 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[0 0 0 0 0 0 0 1];
		reset.
	self assert: decoder nextEncodedUint64 equals: 1.
	
	"SmallInteger maxVal on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[15 255 255 255 255 255 255 255];
		reset.
	self assert: decoder nextEncodedUint64 equals: (2 raisedTo: 60) - 1.
	
	"SmallInteger minVal on 64-bits systems"
	stream
		resetToStart;
		nextPutAll: #[16 0 0 0 0 0 0 0];
		reset.
	self assert: decoder nextEncodedUint64 equals: (2 raisedTo: 60)
]

{ #category : #tests }
FLDecoderTest >> testNextEncodedUint8 [
	stream 
		nextPutAll: #[0];
		reset.
	self assert: decoder nextEncodedUint8 equals: 0.
	
	stream
		resetToStart;
		nextPutAll: #[1];
		reset.
	self assert: decoder nextEncodedUint8 equals: 1.
	
	"Maximum positive 8-bit integer in two's complement"
	stream
		resetToStart;
		nextPutAll: #[255];
		reset.
	self assert: decoder nextEncodedUint8 equals: (2 raisedTo: 8) - 1
]
