"
I check the third party extra fields of ZIP archives (APPNOTE 4.6) understood by ZipArchive: extended timestamp ('UT', 0x5455), Info-ZIP Unix UID and GID ('ux', 0x7875) and Info-ZIP Unicode path ('up', 0x7075).
"
Class {
	#name : 'ZipArchiveExtraFieldsTest',
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
ZipArchiveExtraFieldsTest >> archiveWrittenByInfoZip [
	"Bytes written by Info-ZIP Zip 3.0 on macOS:
	$ zip -q -X- - /etc/hosts
	The member has an extended timestamp (modification time 1793012054), Unix UID and GID 0, and a data descriptor."

	^ ByteArray readHexFrom:
		  '504b0304140008000800c75e5a5d0000000000000000d500000009001c006574632f686f73747355540900035631df6a6f31df6a75780b0001040000000004000000004d8dc10a83300c86cff62902bd9729c8c0b387bd465a635be69ad146866fbfd6c366422eff972fd15a697870119851d06221d5928d1d6ea1c5b1c05e680161709cd6e8f74c2081ea0abf2dba27c424945774d5844fa074d27214a157b32db3c4e40dc0cc9058c0054cbedda89092e4c328ad553fdccdad76dffd7eab611ccd653a9b191787454e3a4d3d5cebef7d01504b0708584845288f000000d5000000504b01021e03140008000800c75e5a5d584845288f000000d5000000090018000000000001000000a481000000006574632f686f73747355540500035631df6a75780b000104000000000400000000504b050600000000010001004f000000e20000000000'
]

{ #category : 'helpers' }
ZipArchiveExtraFieldsTest >> oddSecondTime [

	^ DateAndTime
		  year: 2024
		  month: 5
		  day: 17
		  hour: 10
		  minute: 20
		  second: 31
		  offset: Duration zero
]

{ #category : 'helpers' }
ZipArchiveExtraFieldsTest >> readArchiveDo: aBlock [

	| archive |
	archive := ZipArchive new readFrom: zipFile.
	^ [ aBlock value: archive ] ensure: [ archive close ]
]

{ #category : 'helpers' }
ZipArchiveExtraFieldsTest >> readArchiveFrom: aByteArray do: aBlock [

	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: aByteArray ].
	^ self readArchiveDo: aBlock
]

{ #category : 'running' }
ZipArchiveExtraFieldsTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testClearExtraFields [

	| archive |
	archive := ZipArchive new.
	archive writeExtendedTimestamps: true.
	archive addDeflateString: 'Hello' as: 'hello.txt'.
	self writeArchive: archive.
	self readArchiveDo: [ :readArchive |
			| member |
			member := readArchive members first.
			self deny: member centralDirectoryExtraField isEmpty.
			member clearExtraFields.
			self assert: member localExtraField equals: #[  ].
			self assert: member centralDirectoryExtraField equals: #[  ] ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testExtendedTimestampsAreNotWrittenByDefault [
	"Without extended timestamp, the DOS time has a precision of two seconds"

	| archive bytes |
	archive := ZipArchive new.
	(archive addDeflateString: 'Hello' as: 'hello.txt') modifiedAt: self oddSecondTime.
	bytes := self writeArchive: archive.
	self deny: (bytes asString includesSubstring: 'UT').
	self readArchiveDo: [ :readArchive | self assert: readArchive members first lastModTime equals: self oddSecondTime - 1 second ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testNoUnixOwnerWithoutExtraField [

	| archive |
	archive := ZipArchive new.
	archive addDeflateString: 'Hello' as: 'hello.txt'.
	self writeArchive: archive.
	self readArchiveDo: [ :readArchive |
			self assert: readArchive members first unixUid isNil.
			self assert: readArchive members first unixGid isNil ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testReadsArchiveWithDataDescriptorWrittenByInfoZip [

	self readArchiveFrom: self archiveWrittenByInfoZip do: [ :archive |
			| member |
			member := archive members first.
			self assert: member fileName equals: 'etc/hosts'.
			self assert: member hasDataDescriptor.
			self assert: member contents size equals: 213.
			self assert: (member contents asString beginsWith: '##') ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testReadsExtendedTimestampWrittenByInfoZip [
	"The extended timestamp is in UTC: the result does not depend on the time zone, unlike the DOS time of the headers"

	self
		readArchiveFrom: self archiveWrittenByInfoZip
		do: [ :archive | self assert: archive members first lastModTime equals: (DateAndTime fromUnixTime: 1793012054) ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testReadsUnixOwnerWrittenByInfoZip [

	self readArchiveFrom: self archiveWrittenByInfoZip do: [ :archive |
			self assert: archive members first unixUid equals: 0.
			self assert: archive members first unixGid equals: 0 ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testRewritingKeepsExtendedTimestampUpToDate [
	"An archive read with extended timestamps keeps them when written again, with the current modification time of the members"

	| copyFile |
	copyFile := fs root / 'copy.zip'.
	self readArchiveFrom: self archiveWrittenByInfoZip do: [ :archive |
			archive members first modifiedAt: self oddSecondTime.
			archive writeToFile: copyFile ].
	zipFile := copyFile.
	self readArchiveDo: [ :archive |
			self assert: archive members first lastModTime equals: self oddSecondTime.
			self assert: archive members first unixUid equals: 0 ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testUnicodePathIsIgnoredWhenItDoesNotMatchTheHeaderName [
	"A tool that does not know this block may have renamed the member"

	| archive member block |
	archive := ZipArchive new.
	member := archive addDeflateString: 'Hello' as: 'renamed.txt'.
	block := self unicodePathBlockFor: 'été.txt' replacing: 'XY.txt'.
	member localExtraField: block.
	member centralDirectoryExtraField: block.
	self writeArchive: archive.
	self readArchiveDo: [ :readArchive | self assert: readArchive members first fileName equals: 'renamed.txt' ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testUnicodePathIsUsedWhenItMatchesTheHeaderName [

	| archive member block |
	archive := ZipArchive new.
	member := archive addDeflateString: 'Hello' as: 'XY.txt'.
	block := self unicodePathBlockFor: 'été/日本.txt' replacing: 'XY.txt'.
	member localExtraField: block.
	member centralDirectoryExtraField: block.
	self writeArchive: archive.
	self readArchiveDo: [ :readArchive | self assert: readArchive members first fileName equals: 'été/日本.txt' ]
]

{ #category : 'tests' }
ZipArchiveExtraFieldsTest >> testWritesExtendedTimestamps [

	| archive bytes block |
	archive := ZipArchive new.
	archive writeExtendedTimestamps: true.
	(archive addDeflateString: 'Hello' as: 'hello.txt') modifiedAt: self oddSecondTime.
	bytes := self writeArchive: archive.
	"An 'UT' block of 5 bytes with only the modification time, in the local and central headers"
	block := #[ 16r55 16r54 5 0 1 ].
	self assert: ((1 to: bytes size - 4) count: [ :index | (bytes copyFrom: index to: index + 4) = block ]) equals: 2.
	self readArchiveDo: [ :readArchive | self assert: readArchive members first lastModTime equals: self oddSecondTime ]
]

{ #category : 'helpers' }
ZipArchiveExtraFieldsTest >> unicodePathBlockFor: aName replacing: aHeaderName [

	| crc utf8 |
	crc := CRC crc32FromCollection: aHeaderName asByteArray.
	utf8 := aName utf8Encoded.
	^ #[ 16r75 16r70 ] , {
		  (5 + utf8 size bitAnd: 255).
		  (5 + utf8 size bitShift: -8).
		  1 } asByteArray , ((0 to: 3) collect: [ :index | (crc bitShift: index * -8) bitAnd: 255 ] as: ByteArray) , utf8
]

{ #category : 'helpers' }
ZipArchiveExtraFieldsTest >> writeArchive: anArchive [

	zipFile ensureDelete.
	[ anArchive writeToFile: zipFile ] ensure: [ anArchive close ].
	^ zipFile binaryReadStreamDo: [ :stream | stream upToEnd ]
]
