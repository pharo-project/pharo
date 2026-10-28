"
I test the extraction of whole archives with ZipArchive>>#extractAllTo:informing:overwrite: and its variants: the overwrite flag, the progress reporting, and members that need more than inflating (encrypted members, registered decompressors).

I act as the progress job given to the extraction: I record the titles I am given.
"
Class {
	#name : 'ZipArchiveExtractionTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileSystem',
		'target',
		'progressTitles'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipArchiveExtractionTest >> archiveWith: aString as: aName [

	^ self reread: (ZipArchive new
			   addDeflateString: aString as: aName;
			   yourself)
]

{ #category : 'progress job' }
ZipArchiveExtractionTest >> currentValue: anInteger [

	
]

{ #category : 'helpers' }
ZipArchiveExtractionTest >> reread: anArchive [

	| file |
	file := fileSystem / 'archive.zip'.
	file ensureDelete.
	anArchive writeToFile: file.
	^ ZipArchive new readFrom: file
]

{ #category : 'running' }
ZipArchiveExtractionTest >> setUp [

	super setUp.
	fileSystem := FileSystem memory.
	target := (fileSystem / 'target')
		          ensureCreateDirectory;
		          yourself.
	progressTitles := OrderedCollection new
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testDirectoriesAndNestedFilesAreCreated [

	| archive source |
	source := (fileSystem / 'source')
		          ensureCreateDirectory;
		          yourself.
	archive := ZipArchive new.
	archive addDirectory: source as: 'dir/'.
	archive addDeflateString: 'deep' as: 'dir/sub/deep.txt'.
	(self reread: archive) extractAllTo: target informing: nil overwrite: false.
	self assert: (target / 'dir') isDirectory.
	self assert: (target / 'dir' / 'sub' / 'deep.txt') contents equals: 'deep'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testEncryptedMemberUsesRegisteredDecompressor [
	"Method 199 is registered by ZipArchiveCompressionMethodTest: it reverses the data.
	A palindrome keeps the CRC of the stored data valid."

	| archive bytes member |
	archive := ZipArchive new.
	member := archive addString: 'abcdcba' as: 'p.txt'.
	member desiredCompressionMethod: 0.
	member password: 'pw'.
	bytes := ByteArray streamContents: [ :s | archive writeTo: s ].
	bytes := ZipCryptoTest new patchMethod: 199 flag: 1 in: bytes.
	member := (ZipArchive new readFrom: bytes readStream) members first.
	self assert: member compressionMethod equals: 199.
	member password: 'pw'.
	self assert: member contents asString equals: 'abcdcba'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testEncryptedMembersAreExtractedWithOverwrite [

	| archive |
	archive := ZipArchive new.
	(archive addDeflateString: 'secret text' as: 'secret.txt') password: 'pw'.
	archive := self reread: archive.
	archive password: 'pw'.
	archive extractAllTo: target overwrite: true.
	self assert: (target / 'secret.txt') contents equals: 'secret text'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractAllToInformingKeepsExistingFiles [

	target / 'a.txt' writeStreamDo: [ :s | s nextPutAll: 'old' ].
	(self archiveWith: 'new' as: 'a.txt') extractAllTo: target informing: nil.
	self assert: (target / 'a.txt') contents equals: 'old'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractMemberToFileNamedOutsideTheWorkingDirectory [
	"The caller chose the target file, so it is trusted"

	| file |
	file := fileSystem / 'elsewhere' / 'b.txt'.
	(self archiveWith: 'hello' as: 'a.txt') extractMember: 'a.txt' toFileNamed: file.
	self assert: file contents equals: 'hello'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractMemberWithoutPath [

	| archive |
	archive := self archiveWith: 'read me' as: 'docs/guide/readme.txt'.
	archive extractMemberWithoutPath: 'docs/guide/readme.txt' inDirectory: target.
	self assert: (target children collect: #basename) asArray equals: #( 'readme.txt' ).
	self assert: (target / 'readme.txt') contents equals: 'read me'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractMemberWithoutPathRefusesDotDot [
	"The name comes from the archive: its last part '..' would lead outside of the directory.
	The member is created in the image, since reading an archive canonicalizes names."

	| archive |
	archive := ZipArchive new.
	archive addString: 'hello' as: 'a/..'.
	self should: [ archive extractMemberWithoutPath: 'a/..' inDirectory: target ] raise: ZipArchiveError.
	self assert: (fileSystem root children collect: #basename) asArray equals: #( 'target' )
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractMissingMemberWithoutPathAnswersNil [

	self assert: ((self archiveWith: 'x' as: 'a.txt') extractMemberWithoutPath: 'missing.txt' inDirectory: target) isNil.
	self assert: target children isEmpty
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractToFileNamedInDirectoryTrustsTheGivenName [

	(self archiveWith: 'hello' as: 'a.txt') members first extractToFileNamed: '../out.txt' inDirectory: target overwrite: true.
	self assert: (fileSystem / 'out.txt') contents equals: 'hello'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testExtractWithOverwriteInDirectory [

	target / 'a.txt' writeStreamDo: [ :s | s nextPutAll: 'older and longer' ].
	(self archiveWith: 'new' as: 'a.txt') members first extractWithOverwriteInDirectory: target.
	self assert: (target / 'a.txt') contents equals: 'new'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testOverwriteFalseKeepsExistingFiles [

	target / 'a.txt' writeStreamDo: [ :s | s nextPutAll: 'old' ].
	(self archiveWith: 'new' as: 'a.txt') extractAllTo: target informing: nil overwrite: false.
	self assert: (target / 'a.txt') contents equals: 'old'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testOverwriteTrueReplacesExistingFiles [

	target / 'a.txt' writeStreamDo: [ :s | s nextPutAll: 'older and longer' ].
	(self archiveWith: 'new' as: 'a.txt') extractAllTo: target informing: nil overwrite: true.
	self assert: (target / 'a.txt') contents equals: 'new'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testProgressIsReportedAtMostEvery100Milliseconds [
	"Extracting small members takes far less than 100 ms: only the first one is reported"

	| archive |
	archive := ZipArchive new.
	1 to: 30 do: [ :i | archive addDeflateString: 'x' as: 'file' , i printString , '.txt' ].
	(self reread: archive) extractAllTo: target informing: self overwrite: false.
	self assert: (target / 'file30.txt') exists.
	self assert: progressTitles size < 5 description: progressTitles size printString , ' progress updates'.
	self assert: progressTitles first equals: 'Extracting file1.txt'
]

{ #category : 'tests' }
ZipArchiveExtractionTest >> testUnsupportedMemberLeavesNoFileWithOverwrite [

	| archive |
	archive := self archiveWith: 'data' as: 'a.txt'.
	archive members first instVarNamed: 'compressionMethod' put: 14.
	self should: [ archive extractAllTo: target overwrite: true ] raise: ZipArchiveError.
	self deny: (target / 'a.txt') exists
]

{ #category : 'progress job' }
ZipArchiveExtractionTest >> title: aString [

	progressTitles add: aString
]
