"
I check that split archives (made of several files, APPNOTE 8.5), which are not supported, are detected with a clear ZipArchiveError when reading the last of their files.

I mark an archive as split by changing the disk numbers of its end of central directory record.
"
Class {
	#name : 'ZipArchiveSplitArchiveTest',
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
ZipArchiveSplitArchiveTest >> archiveBytesWithEndOfCentralDirectory: aBlock [
	"Answer the bytes of an archive after aBlock changed them, given the position of the end of central directory signature"

	| archive bytes |
	zipFile ensureDelete.
	archive := ZipArchive new.
	archive addDeflateString: 'Hello' as: 'hello.txt'.
	[ archive writeToFile: zipFile ] ensure: [ archive close ].
	bytes := zipFile binaryReadStreamDo: [ :stream | stream upToEnd ].
	aBlock value: bytes value: ((1 to: bytes size - 3) detect: [ :index | (bytes copyFrom: index to: index + 3) = #[ 80 75 5 6 ] ]).
	^ bytes
]

{ #category : 'helpers' }
ZipArchiveSplitArchiveTest >> readFrom: aByteArray [

	| archive |
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: aByteArray ].
	archive := ZipArchive new readFrom: zipFile.
	archive close.
	^ archive
]

{ #category : 'running' }
ZipArchiveSplitArchiveTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveSplitArchiveTest >> testCentralDirectoryOnAnotherDiskRaisesError [

	| bytes |
	bytes := self archiveBytesWithEndOfCentralDirectory: [ :data :eocd | data at: eocd + 6 put: 1 ].
	self should: [ self readFrom: bytes ] raise: ZipArchiveError
]

{ #category : 'tests' }
ZipArchiveSplitArchiveTest >> testEntriesOnSeveralDisksRaisesError [

	| bytes |
	"2 members in total, but only 1 on this disk"
	bytes := self archiveBytesWithEndOfCentralDirectory: [ :data :eocd | data at: eocd + 10 put: 2 ].
	self should: [ self readFrom: bytes ] raise: ZipArchiveError
]

{ #category : 'tests' }
ZipArchiveSplitArchiveTest >> testLastFileOfSplitArchiveRaisesError [
	"The last file of a split archive in 3 files: disk 2, central directory starting on disk 2"

	| bytes |
	bytes := self archiveBytesWithEndOfCentralDirectory: [ :data :eocd |
			         data at: eocd + 4 put: 2.
			         data at: eocd + 6 put: 2 ].
	self should: [ self readFrom: bytes ] raise: ZipArchiveError
]

{ #category : 'tests' }
ZipArchiveSplitArchiveTest >> testSingleFileArchiveIsRead [

	| bytes |
	bytes := self archiveBytesWithEndOfCentralDirectory: [ :data :eocd | "unchanged" ].
	self assert: (self readFrom: bytes) members size equals: 1
]

{ #category : 'tests' }
ZipArchiveSplitArchiveTest >> testValuesInZip64RecordAreNotConsideredSplit [
	"16rFFFF means that the value is in the Zip64 end of central directory record"

	| bytes |
	bytes := self archiveBytesWithEndOfCentralDirectory: [ :data :eocd |
			         data
				         at: eocd + 8 put: 16rFF;
				         at: eocd + 9 put: 16rFF ].
	self assert: (self readFrom: bytes) members size equals: 1
]
