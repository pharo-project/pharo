"
I check reading and writing Zip64 archives (APPNOTE 4.5.3, 4.3.14 and 4.3.15).

Real Zip64 archives are larger than 4GB or have more than 65534 members, which is too slow for unit tests: I use ZipArchive>>#forceZip64: and an archive written by Python with lowered Zip64 limits instead.
"
Class {
	#name : 'ZipArchiveZip64Test',
	#superclass : 'TestCase',
	#instVars : [
		'fs',
		'zipFile'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipArchiveZip64Test >> assertMembersOf: anArchive [

	| stored deflated |
	stored := anArchive memberNamed: 'stored.txt'.
	deflated := anArchive memberNamed: 'deflated.txt'.
	self assert: stored contents asString equals: 'Pharo'.
	self assert: stored uncompressedSize equals: 5.
	self assert: deflated contents asString equals: 'Hello Hello Hello'.
	self assert: deflated uncompressedSize equals: 17
]

{ #category : 'helpers' }
ZipArchiveZip64Test >> occurrencesOf: aSignature in: aByteArray [

	^ (1 to: aByteArray size - 3) count: [ :index | (aByteArray copyFrom: index to: index + 3) = aSignature ]
]

{ #category : 'helpers' }
ZipArchiveZip64Test >> readArchiveFrom: aByteArray do: aBlock [

	| archive |
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: aByteArray ].
	archive := ZipArchive new readFrom: zipFile.
	^ [ aBlock value: archive ] ensure: [ archive close ]
]

{ #category : 'running' }
ZipArchiveZip64Test >> setUp [

	super setUp.
	fs := FileSystem memory.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testForcedZip64RoundTrip [

	self readArchiveFrom: (self writeArchiveForcingZip64: true) do: [ :archive |
			self assertMembersOf: archive.
			self assert: (archive members allSatisfy: [ :each | each versionNeededToExtract = 45 ]) ]
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testForcedZip64WritesZip64Records [

	| bytes |
	bytes := self writeArchiveForcingZip64: true.
	self assert: (self occurrencesOf: #[ 80 75 6 6 ] in: bytes) equals: 1. "Zip64 end of central directory record"
	self assert: (self occurrencesOf: #[ 80 75 6 7 ] in: bytes) equals: 1. "Zip64 end of central directory locator"
	"Local headers: version needed 45, and both sizes in the Zip64 extended information"
	self assert: (bytes copyFrom: 5 to: 6) equals: #[ 45 0 ].
	self assert: (bytes copyFrom: 19 to: 26) equals: #[ 255 255 255 255 255 255 255 255 ]
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testMissingZip64ExtendedInformationRaisesError [

	| bytes |
	"Rename the Zip64 extended information block (id 1, 24 bytes) of the central directory headers"
	bytes := (self writeArchiveForcingZip64: true) copyReplaceAll: #[ 1 0 24 0 ] with: #[ 16r99 16r99 24 0 ].
	self should: [ self readArchiveFrom: bytes do: [ :archive | archive ] ] raise: ZipArchiveError
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testReadsZip64ArchiveWrittenByPython [

	self readArchiveFrom: self zip64ArchiveWrittenByPython do: [ :archive |
			self assert: (archive members collect: [ :each | each fileName ]) asArray equals: #( 'stored.txt' 'deflated.txt' ).
			self assertMembersOf: archive.
			"The offset of the second local header is only in its Zip64 extended information"
			self assert: (archive memberNamed: 'deflated.txt') localHeaderRelativeOffset equals: 65 ]
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testRewritingZip64ArchiveWithoutZip64 [
	"The Zip64 extended information read from an archive is not copied when it is not needed anymore"

	| copyFile bytes |
	copyFile := fs root / 'copy.zip'.
	self readArchiveFrom: self zip64ArchiveWrittenByPython do: [ :archive | archive writeToFile: copyFile ].
	bytes := copyFile binaryReadStreamDo: [ :stream | stream upToEnd ].
	self assert: (self occurrencesOf: #[ 80 75 6 6 ] in: bytes) equals: 0.
	self assert: (self occurrencesOf: #[ 255 255 255 255 ] in: bytes) equals: 0.
	self readArchiveFrom: bytes do: [ :archive | self assertMembersOf: archive ]
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testWithoutZip64TheFormatIsUnchanged [

	| bytes |
	bytes := self writeArchiveForcingZip64: false.
	self assert: (self occurrencesOf: #[ 80 75 6 6 ] in: bytes) equals: 0.
	self assert: (self occurrencesOf: #[ 80 75 6 7 ] in: bytes) equals: 0.
	self assert: (bytes copyFrom: 5 to: 6) equals: #[ 20 0 ].
	self readArchiveFrom: bytes do: [ :archive | self assertMembersOf: archive ]
]

{ #category : 'tests' }
ZipArchiveZip64Test >> testZip64ArchiveWithPrependedData [
	"Self-extracting archives have data before the first member"

	| archive |
	zipFile ensureDelete.
	archive := ZipArchive new.
	archive forceZip64: true.
	archive addDeflateString: 'Hello Hello Hello' as: 'deflated.txt'.
	archive addString: 'Pharo' as: 'stored.txt'.
	[ zipFile binaryWriteStreamDo: [ :stream | archive writeTo: stream prepending: 'PREPENDED DATA' ] ] ensure: [ archive close ].
	archive := ZipArchive new readFrom: zipFile.
	[ self assertMembersOf: archive ] ensure: [ archive close ]
]

{ #category : 'helpers' }
ZipArchiveZip64Test >> writeArchiveForcingZip64: aBoolean [

	| archive |
	zipFile ensureDelete.
	archive := ZipArchive new.
	archive forceZip64: aBoolean.
	archive addString: 'Pharo' as: 'stored.txt'.
	archive addDeflateString: 'Hello Hello Hello' as: 'deflated.txt'.
	[ archive writeToFile: zipFile ] ensure: [ archive close ].
	^ zipFile binaryReadStreamDo: [ :stream | stream upToEnd ]
]

{ #category : 'helpers' }
ZipArchiveZip64Test >> zip64ArchiveWrittenByPython [
	"Bytes written by Python 3, with Zip64 records for every member and for the archive:
	zipfile.ZIP64_LIMIT = 1
	zipfile.ZIP_FILECOUNT_LIMIT = 1
	with zipfile.ZipFile(buffer, 'w') as w:
		w.writestr(zipfile.ZipInfo('stored.txt', (2024,5,17,10,20,30)), b'Pharo', compress_type=zipfile.ZIP_STORED)
		w.writestr(zipfile.ZipInfo('deflated.txt', (2024,5,17,10,20,30)), b'Hello Hello Hello', compress_type=zipfile.ZIP_DEFLATED)"

	^ #[ 80 75 3 4 45 0 0 0 0 0 143 82 177 88 144 33 178 137 255 255 255 255 255 255 255 255 10 0 20 0 115 116 111 114 101 100 46 116
	     120 116 1 0 16 0 5 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 80 104 97 114 111 80 75 3 4 45 0 0 0 8 0 143 82 177 88 169 26 7 140 255 255
	     255 255 255 255 255 255 12 0 20 0 100 101 102 108 97 116 101 100 46 116 120 116 1 0 16 0 17 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 243
	     72 205 201 201 87 240 64 144 0 80 75 1 2 45 3 45 0 0 0 0 0 143 82 177 88 144 33 178 137 255 255 255 255 255 255 255 255 10
	     0 20 0 0 0 0 0 0 0 0 0 128 1 0 0 0 0 115 116 111 114 101 100 46 116 120 116 1 0 16 0 5 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 80 75
	     1 2 45 3 45 0 0 0 8 0 143 82 177 88 169 26 7 140 255 255 255 255 255 255 255 255 12 0 28 0 0 0 0 0 0 0 0 0 128 1 255 255 255
	     255 100 101 102 108 97 116 101 100 46 116 120 116 1 0 24 0 17 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 65 0 0 0 0 0 0 0 80 75 6 6 44
	     0 0 0 0 0 0 0 45 0 45 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 162 0 0 0 0 0 0 0 137 0 0 0 0 0 0 0 80 75 6 7 0 0 0
	     0 43 1 0 0 0 0 0 0 1 0 0 0 80 75 5 6 0 0 0 0 2 0 2 0 162 0 0 0 137 0 0 0 0 0 ]
]
