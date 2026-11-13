"
I check the DEFLATE compression levels, from 0 (stored blocks, no compression) to 9 (best compression), as in zlib: DeflateStream class>>#on:level:, DeflateStream>>#level: and ZipArchiveMember>>#desiredCompressionLevel:.
"
Class {
	#name : 'DeflateLevelTest',
	#superclass : 'TestCase',
	#instVars : [
		'data'
	],
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'helpers' }
DeflateLevelTest >> gzip: aByteArray level: anInteger [

	| writer |
	writer := GZipWriteStream on: ByteArray new level: anInteger.
	writer nextPutAll: aByteArray.
	writer close.
	^ writer encodedStream contents
]

{ #category : 'running' }
DeflateLevelTest >> setUp [
	"Repetitive data with some noise, larger than the window and than a stored block (65535 bytes)"

	| random |
	super setUp.
	random := Random seed: 7.
	data := ByteArray streamContents: [ :stream |
			        15000 timesRepeat: [
					        stream nextPutAll: (#( 'alpha ' 'beta ' 'gamma ' 'delta ' 'pharo ' 'zip ' ) atRandom: random) asByteArray.
					        stream nextPut: (random nextInteger: 256) - 1 ] ]
]

{ #category : 'tests' }
DeflateLevelTest >> testDefaultLevelIsBestCompression [
	"The level of the streams before levels existed"

	| writer |
	writer := GZipWriteStream on: ByteArray new.
	self assert: writer level equals: 9.
	writer nextPutAll: data.
	writer close.
	self assert: writer encodedStream contents equals: (self gzip: data level: 9)
]

{ #category : 'tests' }
DeflateLevelTest >> testHigherLevelsCompressBetter [

	| sizes |
	sizes := #( 1 6 9 ) collect: [ :level | (self gzip: data level: level) size ].
	self assert: sizes first > sizes second.
	self assert: sizes second > sizes third
]

{ #category : 'tests' }
DeflateLevelTest >> testInvalidLevelIsRejected [

	#( -1 10 nil 2.5 ) do: [ :level | self should: [ GZipWriteStream on: ByteArray new level: level ] raise: Error ]
]

{ #category : 'tests' }
DeflateLevelTest >> testLevelZeroStoresTheData [
	"Stored blocks: 5 bytes of header per block of at most 32768 bytes, plus the GZIP header and trailer"

	| bytes |
	bytes := self gzip: data level: 0.
	self assert: bytes size >= data size.
	self assert: bytes size <= (data size + (5 * (data size // 32768 + 1)) + 18).
	"The first block starts after the 10 bytes of GZIP header: its type is stored (BTYPE = 00)"
	self assert: (((bytes at: 11) bitShift: -1) bitAnd: 3) equals: 0
]

{ #category : 'tests' }
DeflateLevelTest >> testRoundTripAtEveryLevel [

	0 to: 9 do: [ :level | self assert: (GZipReadStream on: (self gzip: data level: level)) upToEnd equals: data ]
]

{ #category : 'tests' }
DeflateLevelTest >> testZLibAtEveryLevel [

	0 to: 9 do: [ :level |
			| writer |
			writer := ZLibWriteStream on: ByteArray new level: level.
			writer nextPutAll: data.
			writer close.
			self assert: (ZLibReadStream on: writer encodedStream contents) upToEnd equals: data ]
]

{ #category : 'tests' }
DeflateLevelTest >> testZipMemberUsesDesiredCompressionLevel [

	| sizes |
	sizes := #( 1 9 ) collect: [ :level |
			         | archive stream |
			         archive := ZipArchive new.
			         (archive addString: data asString as: 'data.bin') desiredCompressionLevel: level.
			         stream := ByteArray new writeStream.
			         archive writeTo: stream.
			         archive close.
			         stream contents size ].
	self assert: sizes first > sizes last.
	self assert: sizes last < (data size // 2)
]

{ #category : 'tests' }
DeflateLevelTest >> testZipMemberWithLevelZeroIsStored [

	| archive member |
	archive := ZipArchive new.
	member := archive addString: 'Hello Hello Hello' as: 'hello.txt'.
	member desiredCompressionLevel: 0.
	self assert: member desiredCompressionMethod equals: 0.
	archive close
]
