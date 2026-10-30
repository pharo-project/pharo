"
I check how ZipArchive handles compression methods other than stored and deflated (APPNOTE 4.4.5): a clear ZipArchiveError instead of garbage, raw copy when an archive is written again, and decompressors registered with the pragma <zipDecompressorForMethod:>.

I register a toy decompressor for method 199, which is not assigned by APPNOTE: its data is the stored data in reverse order.
"
Class {
	#name : 'ZipArchiveCompressionMethodTest',
	#superclass : 'TestCase',
	#instVars : [
		'fs',
		'zipFile'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'decompressors' }
ZipArchiveCompressionMethodTest class >> reverseDecompress: aByteArray [
	"Toy decompressor for the tests: method 199 stores data in reverse order"

	<zipDecompressorForMethod: 199>
	^ aByteArray reversed
]

{ #category : 'helpers' }
ZipArchiveCompressionMethodTest >> archiveWithMethod: aMethod data: dataBytes for: aString [
	"Answer the bytes of an archive with a stored member for aString, changed to use the compression method aMethod and the given data of the same size. The CRC-32 is still the one of aString."

	| archive bytes |
	zipFile ensureDelete.
	archive := ZipArchive new.
	archive addString: aString as: 'member.txt'.
	[ archive writeToFile: zipFile ] ensure: [ archive close ].
	bytes := zipFile binaryReadStreamDo: [ :stream | stream upToEnd ].
	bytes := bytes copyReplaceAll: aString asByteArray with: dataBytes.
	"compression method: 8 bytes after the local header signature, 10 bytes after the central one"
	bytes at: 9 put: aMethod.
	bytes at: (self centralDirectoryStartIn: bytes) + 10 put: aMethod.
	^ bytes
]

{ #category : 'helpers' }
ZipArchiveCompressionMethodTest >> centralDirectoryStartIn: aByteArray [

	^ (1 to: aByteArray size - 3) detect: [ :index | (aByteArray copyFrom: index to: index + 3) = #[ 80 75 1 2 ] ]
]

{ #category : 'helpers' }
ZipArchiveCompressionMethodTest >> readArchiveFrom: aByteArray do: aBlock [

	| archive |
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: aByteArray ].
	archive := ZipArchive new readFrom: zipFile.
	^ [ aBlock value: archive ] ensure: [ archive close ]
]

{ #category : 'running' }
ZipArchiveCompressionMethodTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testCompressionMethodNames [

	self assert: (ZipArchiveMember compressionMethodNameFor: 0) equals: 'stored'.
	self assert: (ZipArchiveMember compressionMethodNameFor: 8) equals: 'deflated'.
	self assert: (ZipArchiveMember compressionMethodNameFor: 93) equals: 'Zstandard'.
	self assert: (ZipArchiveMember compressionMethodNameFor: 199) equals: 'unknown'
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testRegisteredDecompressorChecksTheCrc [

	| bytes |
	"Not reversed: the decompressed data is 'olleH', whose CRC-32 is not the one of 'Hello'"
	bytes := self archiveWithMethod: 199 data: 'Hello' asByteArray for: 'Hello'.
	self readArchiveFrom: bytes do: [ :archive | self should: [ archive members first contents ] raise: CRCError ]
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testRegisteredDecompressorIsUsed [

	| bytes |
	bytes := self archiveWithMethod: 199 data: 'olleH' asByteArray for: 'Hello'.
	self readArchiveFrom: bytes do: [ :archive |
			self assert: archive members first canDecompress.
			self assert: archive members first contents asString equals: 'Hello' ]
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testUnsupportedMethodIsCopiedWhenWritingAgain [

	| bytes copyFile |
	bytes := self archiveWithMethod: 14 data: 'olleH' asByteArray for: 'Hello'.
	copyFile := fs root / 'copy.zip'.
	self readArchiveFrom: bytes do: [ :archive | archive writeToFile: copyFile ].
	self readArchiveFrom: (copyFile binaryReadStreamDo: [ :stream | stream upToEnd ]) do: [ :archive |
			self assert: archive members first compressionMethod equals: 14.
			self assert: archive members first uncompressedSize equals: 5 ]
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testUnsupportedMethodIsNotExtractedToAFile [

	| bytes target |
	bytes := self archiveWithMethod: 14 data: 'olleH' asByteArray for: 'Hello'.
	target := (fs root / 'target')
		          ensureCreateDirectory;
		          yourself.
	self
		readArchiveFrom: bytes
		do: [ :archive | self should: [ archive members first extractInDirectory: target ] raise: ZipArchiveError ].
	self deny: (target / 'member.txt') exists
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testUnsupportedMethodRaisesClearError [

	| bytes |
	bytes := self archiveWithMethod: 14 data: 'olleH' asByteArray for: 'Hello'.
	self readArchiveFrom: bytes do: [ :archive |
			| member message |
			member := archive members first.
			self assert: member compressionMethodName equals: 'LZMA'.
			self deny: member canDecompress.
			message := [
				           member contents.
				           nil ]
				           on: ZipArchiveError
				           do: [ :error | error messageText ].
			self assert: (message includesSubstring: '14 (LZMA)') ]
]

{ #category : 'tests' }
ZipArchiveCompressionMethodTest >> testWritingWithUnsupportedMethodRaisesError [
	"It would write stored data labelled with another method"

	| archive |
	archive := ZipArchive new.
	(archive addString: 'Hello' as: 'member.txt') desiredCompressionMethod: 14.
	self should: [ archive writeToFile: zipFile ] raise: ZipArchiveError.
	archive close
]
