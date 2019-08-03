"
Unit tests for ZipWriteStream
"
Class {
	#name : #ZipWriteStreamTest,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipWriteStreamTest >> testEmptyStrings [
	| aStream zipStream |
	aStream := ByteArray new writeStream binary.
	zipStream := ZipWriteStream on: aStream.
	zipStream nextStringPut: ''.
	zipStream nextStringPut: ''.
	zipStream close.
	aStream close.
	aStream := aStream contents readStream binary.
	zipStream := ZipReadStream on: aStream.
	self assertEmpty: zipStream nextString.
	self assertEmpty: zipStream nextString.
	zipStream close.
	aStream close
]

{ #category : #tests }
ZipWriteStreamTest >> testZipped [
	| compressed |
	
	compressed := 'hello' zipped.
	self assert: compressed unzipped equals: 'hello'
]
