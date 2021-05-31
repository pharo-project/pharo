Class {
	#name : #FLEncoderDecoderTest,
	#superclass : #TestCase,
	#instVars : [
		'encoder',
		'stream',
		'decoder'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLEncoderDecoderTest >> assertIdentityOf: aSmallInteger using: anArray [
	stream resetToStart.
	
	encoder
		perform: anArray first with: aSmallInteger;
		flush.
	stream reset.
	self assert: (decoder perform: anArray second) identicalTo: aSmallInteger
]

{ #category : #running }
FLEncoderDecoderTest >> setUp [
	super setUp.
	
	stream := ReadWriteStream on: ByteArray new.
	encoder := FLEncoder
		on: stream
		globalEnvironment: self class environment.
	
	decoder := FLDecoder
		on: stream
		migrations: #()
		globalEnvironment: self class environment
	
]

{ #category : #tests }
FLEncoderDecoderTest >> testSmallIntegerSerializationForAllBitsWith16Bits [
	1
		to: 15
		do: [ :exp |
			| integer |
			integer := 2 raisedTo: exp.
			self
				assertIdentityOf: integer
				using: #(encodeUint16: nextEncodedUint16) ]
]

{ #category : #tests }
FLEncoderDecoderTest >> testSmallIntegerSerializationForAllBitsWith24Bits [
	1
		to: 23
		do: [ :exp |
			| integer |
			integer := 2 raisedTo: exp.
			self
				assertIdentityOf: integer
				using: #(encodeUint24: nextEncodedUint24).
			integer < (2 raisedTo: 23) ifTrue: [
				self
					assertIdentityOf: integer
					using: #(encodeInt24: nextEncodedInt24) ].
			integer := 0 - integer.
			self
				assertIdentityOf: integer
				using: #(encodeInt24: nextEncodedInt24) ]
]

{ #category : #tests }
FLEncoderDecoderTest >> testSmallIntegerSerializationForAllBitsWith32Bits [
	1
		to: 30
		do: [ :exp |
			| integer |
			integer := 2 raisedTo: exp.
			integer isLarge ifFalse: [
				self
					assertIdentityOf: integer
					using: #(encodeUint32: nextEncodedUint32).
			integer < (2 raisedTo: 30) ifTrue: [
				self
					assertIdentityOf: integer
					using: #(encodeInt32: nextEncodedInt32) ] ].
			integer := 0 - integer.
			integer isLarge ifFalse: [
				self
					assertIdentityOf: integer
					using: #(encodeInt32: nextEncodedInt32) ] ]
]

{ #category : #tests }
FLEncoderDecoderTest >> testSmallIntegerSerializationForAllBitsWith64Bits [
	1
		to: 60
		do: [ :exp |
			| integer |
			integer := 2 raisedTo: exp.
			integer isLarge ifFalse: [
				self
					assertIdentityOf: integer
					using: #(encodeUint64: nextEncodedUint64).
			integer < (2 raisedTo: 60) ifTrue: [
				self
					assertIdentityOf: integer
					using: #(encodeInt64: nextEncodedInt64) ] ].
			integer := 0 - integer.
			integer isLarge ifFalse: [
				self
					assertIdentityOf: integer
					using: #(encodeInt64: nextEncodedInt64) ] ]
]

{ #category : #tests }
FLEncoderDecoderTest >> testSmallIntegerSerializationForAllBitsWith8Bits [
	1
		to: 7
		do: [ :exp |
			| integer |
			integer := 2 raisedTo: exp.
			self
				assertIdentityOf: integer
				using: #(encodeUint8: nextEncodedUint8) ]
]
