"
I am a stream decorator that knows how to read and write little endian numbers from my underlying stream.
"
Class {
	#name : #ZnEndianessReadWriteStream,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnEndianessReadWriteStream class >> on: writeStream [
	^ self basicNew
		on: writeStream;
		yourself
]

{ #category : #'instance creation' }
ZnEndianessReadWriteStream class >> on: writeStream do: block [
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result
]

{ #category : #endianess }
ZnEndianessReadWriteStream >> nextLittleEndianNumber: n [ 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := stream next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
]

{ #category : #endianess }
ZnEndianessReadWriteStream >> nextLittleEndianNumber: n put: value [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value byteAt: i)].
	stream nextPutAll: bytes
]

{ #category : #'initialize-release' }
ZnEndianessReadWriteStream >> on: aStream [

	stream := aStream
]
