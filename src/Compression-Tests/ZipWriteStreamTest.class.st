"
Unit tests for ZipWriteStream
"
Class {
	#name : 'ZipWriteStreamTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'tests' }
ZipWriteStreamTest >> testDeflateTablesFollowRFC1951 [
	"RFC 1951, 3.2.5. Lengths and distances are stored minus their minimum (3 and 1)"

	self assert: ZipWriteStream maxLiteralCodes equals: 286.
	self assert: ZipWriteStream maxDistanceCodes equals: 30.
	self
		assert: (ZipWriteStream baseLength first: 28) asArray
		equals: #( 0 1 2 3 4 5 6 7 8 10 12 14 16 20 24 28 32 40 48 56 64 80 96 112 128 160 192 224 ).
	self assert: ZipWriteStream extraLengthBits asArray equals: #( 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 ).
	self
		assert: ZipWriteStream baseDistance asArray
		equals: #( 0 1 2 3 4 6 8 12 16 24 32 48 64 96 128 192 256 384 512 768 1024 1536 2048 3072 4096 6144 8192 12288 16384 24576 ).
	self
		assert: ZipWriteStream extraDistanceBits asArray
		equals: #( 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 ).
	"Code of each match length from 3 to 258, and of each of the first 256 distances"
	self assert: ZipWriteStream matchLengthCodes size equals: 256.
	self assert: ZipWriteStream matchLengthCodes first equals: 257.
	self assert: (ZipWriteStream matchLengthCodes at: 257 - 3 + 1) equals: 284.
	self assert: ZipWriteStream matchLengthCodes last equals: 285.
	self assert: ZipWriteStream distanceCodes size equals: 512.
	self assert: (ZipWriteStream distanceCodes first: 5) asArray equals: #( 0 1 2 3 4 )
]

{ #category : 'tests' }
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

{ #category : 'tests' }
ZipWriteStreamTest >> testLongestMatchesRoundTrip [
	"A long run of one byte is made of matches of the longest length, 258, encoded with code 285 and no extra bits (RFC 1951)"

	| data compressed |
	data := ByteArray new: 100000 withAll: 97.
	compressed := ByteArray streamContents: [ :stream |
			              (ZipWriteStream on: stream)
				              nextPutAll: data;
				              close ].
	self assert: (InflateStream on: compressed readStream) upToEnd asByteArray equals: data.
	"With code 284 and 31 extra bits instead, it took 358 bytes"
	self assert: compressed size < 200
]

{ #category : 'tests' }
ZipWriteStreamTest >> testZipped [
	| compressed |

	compressed := 'hello' zipped.
	self assert: compressed unzipped equals: 'hello'
]
