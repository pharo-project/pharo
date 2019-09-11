"
Tests for correct CRC handling in ZipWriteStream
"
Class {
	#name : #ZipCrcTest,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipCrcTest >> testInvalidGZipCrc [
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-5. "before the length"
	bytes byteAt: bytes size-5 put: (crcByte + 1 bitAnd: 255).

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.

]

{ #category : #tests }
ZipCrcTest >> testInvalidZLibCrc [
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-2.
	bytes byteAt: bytes size-2 put: (crcByte + 1 bitAnd: 255).

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.

]

{ #category : #tests }
ZipCrcTest >> testInvalidZipCrc [
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader contents] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader next: 100] raise: CRCError.

]

{ #category : #tests }
ZipCrcTest >> testMissingGZipCrc [
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	bytes := bytes copyFrom: 1 to: bytes size-6.

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.

]

{ #category : #tests }
ZipCrcTest >> testMissingZLibCrc [
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	bytes := bytes copyFrom: 1 to: bytes size-2.

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.

]

{ #category : #tests }
ZipCrcTest >> testMissingZipCrc [
	"See that the lack of a CRC does not raise an error"

	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.
	bytes := writer encodedStream contents.
	reader := ZipReadStream on: bytes.
	readBytes := reader upToEnd.
	self assert: readBytes equals: 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader contents.
	reader := ZipReadStream on: bytes.
	reader next: 100
]

{ #category : #tests }
ZipCrcTest >> testValidGZipCrc [
	| reader writer bytes |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := GZipReadStream on: bytes.
	self assert: reader upToEnd equals: 'Hello World'.
]

{ #category : #tests }
ZipCrcTest >> testValidZLibCrc [
	| reader writer bytes |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: reader upToEnd equals: 'Hello World'.
	
	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: (reader next: 100) equals: 'Hello World'.
]

{ #category : #tests }
ZipCrcTest >> testValidZipCrc [
	"See that a correct CRC does not raise an error and that we can read what we wrote."

	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.
	bytes := writer encodedStream contents.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader upToEnd.
	self assert: readBytes equals: 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader contents.
	self assert: readBytes equals: 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader next: 11.
	self assert: readBytes equals: 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader next: 100.
	self assert: readBytes equals: 'Hello World'
]
