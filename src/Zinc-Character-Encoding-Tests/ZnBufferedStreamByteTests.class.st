"
Tests for bufferer read/write streams that are assumed to contain bytes.

Right now, integer encoding/decoding

References

https://en.wikipedia.org/wiki/Endianness
https://en.wikipedia.org/wiki/Two%27s_complement
"
Class {
	#name : #ZnBufferedStreamByteTests,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #accessing }
ZnBufferedStreamByteTests >> integerEncodingSpec [
	^ #(
"<hex-bytes> <integer> <u|s> <be|le> unsigned|signed big-endian|little-endian"
('00' 0 u be)
('00' 0 s be)
('01' 1 u be)
('01' 1 s be)
('FF' -1 s be)
('FF' 255 u be)
('7B' 123 u be)
('7B' 123 s be)
('85' -123 s be)
('7F' 127 u be)
('7F' 127 s be)
('80' -128 s be)
('00' 0 u le)
('00' 0 s le)
('01' 1 u le)
('01' 1 s le)
('FF' -1 s le)
('FF' 255 u le)
('7B' 123 u le)
('7B' 123 s le)
('85' -123 s le)
('7F' 127 u le)
('7F' 127 s le)
('80' -128 s le)
('00000000' 0 u be)
('00000000' 0 s be)
('00000000' 0 u le)
('00000000' 0 s le)
('00000001' 1 u be)
('00000001' 1 s be)
('01000000' 1 u le)
('01000000' 1 s le)
('FFFFFFFF' -1 s be)
('FFFFFFFF' -1 s le)
('000004D2' 1234 u be)
('000004D2' 1234 s be)
('FFFFFB2E' -1234 s be)
('D2040000' 1234 u le)
('D2040000' 1234 s le)
('2EFBFFFF' -1234 s le)
('FFFFFFFF' 4294967295 u be)
('FFFFFFFF' 4294967295 u le)
('7FFFFFFF' 2147483647 u be)
('7FFFFFFF' 2147483647 s be)
('80000000' -2147483648 s be)
('FFFFFF7F' 2147483647 s le)
('00000080' -2147483648 s le)
('499602D2' 1234567890 u be)
('499602D2' 1234567890 s be)
('B669FD2E' -1234567890 s be)
('D2029649' 1234567890 u le)
('D2029649' 1234567890 s le)
('2EFD69B6' -1234567890 s le)
)

]

{ #category : #testing }
ZnBufferedStreamByteTests >> testInt16Aliases [
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '04D2'.
	self assert: (ZnBufferedReadStream on: input readStream) int16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStream) uint16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStream) nextWord equals: 1234.
	self assert: ((ZnBufferedReadStream on: input readStream) nextNumber: 2) equals: 1234.
	self assert: ((ZnBufferedReadStream on: input reversed readStream) nextLittleEndianNumber: 2) equals: 1234.	
	self assert: (writer value: [ :out | out int16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out uint16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextWordPut: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 2 put: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 2 put: 1234 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'FB2E'.
	self assert: (ZnBufferedReadStream on: input readStream) int16 equals: -1234.
	self assert: (writer value: [ :out | out int16: -1234 ]) equals: input
]

{ #category : #testing }
ZnBufferedStreamByteTests >> testInt32Aliases [
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '499602D2'.
	self assert: (ZnBufferedReadStream on: input readStream) int32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStream) uint32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStream) nextInt32 equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input readStream) nextNumber: 4) equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input reversed readStream) nextLittleEndianNumber: 4) equals: 1234567890.	
	self assert: (writer value: [ :out | out int32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out uint32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 4 put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 4 put: 1234567890 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'B669FD2E'.
	self assert: (ZnBufferedReadStream on: input readStream) int32 equals: -1234567890.
	self assert: (ZnBufferedReadStream on: input readStream) nextInt32 equals: -1234567890.
	self assert: (writer value: [ :out | out int32: -1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: -1234567890 ]) equals: input
]

{ #category : #testing }
ZnBufferedStreamByteTests >> testInt8 [
	| bytes readStream |
	bytes := ByteArray streamContents: [ :out |
		ZnBufferedWriteStream on: out do: [ :bout |
			bout int8: 123; uint8: 123; int8: -123 ] ].
	readStream := ZnBufferedReadStream on: bytes readStream.
	self assert: readStream peek equals: 123.
	self assert: readStream int8 equals: 123.
	self assert: readStream peek equals: 123.
	self assert: readStream uint8 equals: 123.
	self deny: readStream peek = 123.
	self deny: readStream peek = -123.
	self assert: readStream int8 equals: -123
]

{ #category : #testing }
ZnBufferedStreamByteTests >> testNextIntegerOfSizeSignedBigEndian [
	self integerEncodingSpec do: [ :spec |
		| input integer |
		input := ByteArray readHexFrom: spec first.
		integer := (ZnBufferedReadStream on: input readStream) 
			nextIntegerOfSize: input 
			size signed: spec third = #s 
			bigEndian: spec fourth = #be.
		self assert: integer equals: spec second ]
]

{ #category : #testing }
ZnBufferedStreamByteTests >> testNextIntegerOfSizeSignedBigEndianPut [
	self integerEncodingSpec do: [ :spec |
		| input output |
		input := ByteArray readHexFrom: spec first.
		output := ByteArray streamContents: [ :out |
			ZnBufferedWriteStream on: out do: [ :bout | 
				bout 
					nextIntegerOfSize: input size 
					signed: spec third = #s 
					bigEndian: spec fourth = #be 
					put: spec second ] ].
		self assert: output equals: input ]
]
