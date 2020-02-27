"
ZipReadStream is intended for uncompressing the compressed contents of Zip archive members.

Since Zip archive members keep their expected CRC value separately in Zip headers, this class does not attempt to read the CRC from its input stream.

Instead, if you want the CRC verification to work you have to call #expectedCrc: with the expected CRC-32 value from the Zip member header.
"
Class {
	#name : #ZipReadStream,
	#superclass : #FastInflateStream,
	#instVars : [
		'expectedCrc'
	],
	#category : #'Compression-Streams'
}

{ #category : #crc }
ZipReadStream >> expectedCrc: aNumberOrNil [
	"If expectedCrc is set, it will be compared against the calculated CRC32 in verifyCrc.
	This number should be the number read from the Zip header (which is the bitwise complement of my crc if all is working correctly)"
	expectedCrc := aNumberOrNil
]

{ #category : #initialization }
ZipReadStream >> on: aCollection from: firstIndex to: lastIndex [
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 16rFFFFFFFF.
	expectedCrc := nil.
]

{ #category : #crc }
ZipReadStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection
]

{ #category : #crc }
ZipReadStream >> verifyCrc [
	"Verify the CRC-32 checksum calculated from the input against the expected CRC-32, if any.
	Answer the calculated CRC-32 in any case.
	Note that the CRC-32 used in Zip files is actually the bit inverse of the calculated value, so that is what is returned."

	| invertedCrc |
	invertedCrc := crc bitXor: 16rFFFFFFFF.
	(expectedCrc notNil and: [ expectedCrc ~= invertedCrc ])
		ifTrue: [ ^ self crcError: ('Wrong CRC-32 (expected {1} got {2}) (proceed to ignore)' format: { expectedCrc printStringHex. invertedCrc printStringHex }) ].
	^invertedCrc
]
