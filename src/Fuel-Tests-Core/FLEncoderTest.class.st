Class {
	#name : #FLEncoderTest,
	#superclass : #TestCase,
	#instVars : [
		'encoder',
		'stream'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #helpers }
FLEncoderTest >> getResult [
	| contents|
	encoder flush.
	contents := stream contents.
	stream reset.
	^ contents
]

{ #category : #running }
FLEncoderTest >> setUp [
	super setUp.
	
	stream := WriteStream on: ByteArray new.
	encoder := FLEncoder
		on: stream
		globalEnvironment: self class environment
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt24 [
	encoder encodeInt24: 0.
	self assert: self getResult equals: #[0 0 0].
	
	encoder encodeInt24: 1.
	self assert: self getResult equals: #[0 0 1].
	
	"-1 in two's complement"
	encoder encodeInt24: (2 raisedTo: 23) - 1.
	self assert: self getResult equals: #[127 255 255].
	
	"Maximum positive 24-bit integer in two's complement"
	encoder encodeInt24: (2 raisedTo: 22) - 1.
	self assert: self getResult equals: #[63 255 255].
	
	"Minimum negative 24-bit integer in two's complement"
	encoder encodeInt24: (2 raisedTo: 22).
	self assert: self getResult equals: #[64 0 0]
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt24Truncated [
	encoder encodeInt24: (2 raisedTo: 24).
	self assert: self getResult equals: #[0 0 0].
	
	encoder encodeInt24: (2 raisedTo: 32) - 1.
	self assert: self getResult equals: #[255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt32 [
	"Note: SmallInteger in Pharo is 31 / 63 bits wide and stored in two's complement format.
			 Hence, the largest positive values are 2^30 - 1 and 2^62 - 1, the smallest negative
			 values -2^30 and -2^63, respectively."
	encoder encodeInt32: 0.
	self assert: self getResult equals: #[0 0 0 0].
	
	encoder encodeInt32: 1.
	self assert: self getResult equals: #[0 0 0 1].
	
	"-1 on 32-bits systems"
	encoder encodeInt32: (2 raisedTo: 31) - 1.
	self assert: self getResult equals: #[127 255 255 255].
	
	"SmallInteger maxVal on 32-bits systems"
	encoder encodeInt32: (2 raisedTo: 30) - 1.
	self assert: self getResult equals: #[63 255 255 255].
	
	"SmallInteger minVal on 32-bits systems"
	encoder encodeInt32: (2 raisedTo: 30).
	self assert: self getResult equals: #[64 0 0 0]
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt32Truncated [
	encoder encodeInt32: (2 raisedTo: 32).
	self assert: self getResult equals: #[0 0 0 0].
	
	encoder encodeInt32: (2 raisedTo: 64) - 1.
	self assert: self getResult equals: #[127 255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt64 [
	"Note: SmallInteger in Pharo is 31 / 63 bits wide and stored in two's complement format.
			 Hence, the largest positive values are 2^30 - 1 and 2^62 - 1, the smallest negative
			 values -2^30 and -2^63, respectively."
	encoder encodeInt64: 0.
	self assert: self getResult equals: #[0 0 0 0 0 0 0 0].
	
	encoder encodeInt64: 1.
	self assert: self getResult equals: #[0 0 0 0 0 0 0 1].
	
	"-1 on 64-bits systems"
	encoder encodeInt64: -1.
	self assert: self getResult equals: #[31 255 255 255 255 255 255 255].
	
	"SmallInteger maxVal on 32-bits systems"
	encoder encodeInt64: (2 raisedTo: 60) - 1.
	self assert: self getResult equals: #[15 255 255 255 255 255 255 255].
	
	"SmallInteger minVal on 64-bits systems"
	encoder encodeInt64: 0 - (2 raisedTo: 60).
	self assert: self getResult equals: #[16 0 0 0 0 0 0 0]
]

{ #category : #tests }
FLEncoderTest >> testEncodeInt64Truncated [
	encoder encodeInt64: (2 raisedTo: 64).
	self assert: self getResult equals: #[0 0 0 0 0 0 0 0].
	
	encoder encodeInt64: (2 raisedTo: 65) - 1.
	self assert: self getResult equals: #[31 255 255 255 255 255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUInt8 [
	encoder encodeUint8: 0.
	self assert: self getResult equals: #[0].
	
	encoder encodeUint8: 1.
	self assert: self getResult equals: #[1].
	
	"Maximum positive 8-bit integer"
	encoder encodeUint8: (2 raisedTo: 8) - 1.
	self assert: self getResult equals: #[255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUInt8Truncated [
	encoder encodeUint8: (2 raisedTo: 8).
	self assert: self getResult equals: #[0].
	
	encoder encodeUint8: (2 raisedTo: 16) - 1.
	self assert: self getResult equals: #[255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint16 [
	encoder encodeUint16: 0.
	self assert: self getResult equals: #[0 0].
	
	encoder encodeUint16: 1.
	self assert: self getResult equals: #[0 1].
	
	"Maximum positive 16-bit integer"
	encoder encodeUint16: (2 raisedTo: 16) - 1.
	self assert: self getResult equals: #[255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint16Truncated [
	encoder encodeUint16: (2 raisedTo: 16).
	self assert: self getResult equals: #[0 0].
	
	encoder encodeUint16: (2 raisedTo: 32) - 1.
	self assert: self getResult equals: #[255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint24 [
	encoder encodeUint24: 0.
	self assert: self getResult equals: #[0 0 0].
	
	encoder encodeUint24: 1.
	self assert: self getResult equals: #[0 0 1].
	
	"Maximum positive 24-bit integer"
	encoder encodeUint24: (2 raisedTo: 24) - 1.
	self assert: self getResult equals: #[255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint24Truncated [
	encoder encodeUint24: (2 raisedTo: 24).
	self assert: self getResult equals: #[0 0 0].
	
	encoder encodeUint24: (2 raisedTo: 24) - 1.
	self assert: self getResult equals: #[255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint32 [
	encoder encodeUint32: 0.
	self assert: self getResult equals: #[0 0 0 0].
	
	encoder encodeUint32: 1.
	self assert: self getResult equals: #[0 0 0 1].
	
	"Maximum positive 32-bit integer"
	encoder encodeUint32: (2 raisedTo: 32) - 1.
	self assert: self getResult equals: #[255 255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint32Truncated [
	encoder encodeUint32: (2 raisedTo: 32).
	self assert: self getResult equals: #[0 0 0 0].
	
	encoder encodeUint32: (2 raisedTo: 64) - 1.
	self assert: self getResult equals: #[255 255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint64 [
	encoder encodeUint64: 0.
	self assert: self getResult equals: #[0 0 0 0 0 0 0 0].
	
	encoder encodeUint64: 1.
	self assert: self getResult equals: #[0 0 0 0 0 0 0 1].
	
	"Maximum positive 64-bit integer"
	encoder encodeUint64: (2 raisedTo: 63) - 1.
	self assert: self getResult equals: #[127 255 255 255 255 255 255 255]
]

{ #category : #tests }
FLEncoderTest >> testEncodeUint64Truncated [
	encoder encodeUint64: (2 raisedTo: 64).
	self assert: self getResult equals: #[0 0 0 0 0 0 0 0].
	
	encoder encodeUint64: (2 raisedTo: 65) - 1.
	self assert: self getResult equals: #[255 255 255 255 255 255 255 255]
]
