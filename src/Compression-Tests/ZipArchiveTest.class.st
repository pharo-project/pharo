"
SUnit tests for ZIP archives
"
Class {
	#name : 'ZipArchiveTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileToZip',
		'zip',
		'subdir',
		'zipFile',
		'fs'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'accessing' }
ZipArchiveTest class >> defaultTimeLimit [
	^30 seconds
]

{ #category : 'tests' }
ZipArchiveTest >> assertLastModificationTimeFrom: endianStream expected: fileModified [
	| lastModificationTime |
	lastModificationTime := DateAndTime fromDosTimestamp: (endianStream nextLittleEndianNumber: 4).
	self assert:
			(lastModificationTime
				between: fileModified - 2 seconds
				and: fileModified + 2 seconds)
]

{ #category : 'tests - members' }
ZipArchiveTest >> reread: anArchive [

	^ ZipArchive new readFrom: (ByteArray streamContents: [ :stream | anArchive writeTo: stream ]) readStream
]

{ #category : 'running' }
ZipArchiveTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	fileToZip := fs root / 'test-zip-file'.
	fileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	subdir := fs root / 'test-zip-dir'.
	subdir ensureCreateDirectory.
	zipFile := fs root / ('pharo_test' , '.zip').
	zip := ZipArchive new
]

{ #category : 'running' }
ZipArchiveTest >> tearDown [
	"Must close the zipfile before deleting all files.
	Otherwise in windows the file will not be deleted as long as the file is open."

	[ zip close ]
		ensure: [ fileToZip ensureDelete.
			subdir ensureDeleteAll.
			zipFile ensureDelete ].
	super tearDown
]

{ #category : 'tests - adding' }
ZipArchiveTest >> testAddDirectoryUsesTheBasename [

	self assert: (zip addDirectory: subdir) fileName equals: 'test-zip-dir/'.
	self assert: (zip addDirectory: subdir as: 'other/') fileName equals: 'other/'.
	self assert: (zip addDirectory: subdir as: 'another') fileName equals: 'another/'
]

{ #category : 'tests - adding' }
ZipArchiveTest >> testAddFileUsesTheBasename [

	| member |
	member := zip addFile: fileToZip.
	self assert: member fileName equals: 'test-zip-file'
]

{ #category : 'tests' }
ZipArchiveTest >> testAddNonExistentFile [

	self should: [ zip addFile: 'it_would_be_crazy_if_this_file_existed.ext' asFileReference ] raise: FileDoesNotExistException
]

{ #category : 'tests' }
ZipArchiveTest >> testAddTreeWhenNoContentInFileShouldSucceed [
	| zipOutput  extractionDir |
	(subdir/'testZipFileInDir') ensureCreateFile.
	(subdir/'testZipFileInDir2') ensureCreateFile.
	(subdir/'DeeperDirectory') ensureCreateDirectory.
	(subdir/'DeeperDirectory'/'testZipFileInDir3')  ensureCreateFile.
	extractionDir := fs root / 'extracted'.
	extractionDir ensureCreateDirectory.

	zipOutput := zip addTree: subdir relativeTo: (subdir parent) match: [ :e | true ].

	zipOutput extractAllTo: extractionDir.
	self assert:	 (extractionDir / 'test-zip-dir'/'testZipFileInDir') asFileReference isFile.
	self assert:	 (extractionDir / 'test-zip-dir'/'testZipFileInDir2') asFileReference isFile.
	self assert:	 (extractionDir / 'test-zip-dir'/'DeeperDirectory'/'testZipFileInDir3') asFileReference isFile
]

{ #category : 'tests' }
ZipArchiveTest >> testAddTreeWithContentInFileShouldSucceed [
	| zipOutput  extractionDir fileWithContentToZip fileContent extractedFile |
	fileContent := 'some random text '.
	extractionDir := fs root / 'extracted'.
	extractionDir ensureCreateDirectory.
	fileWithContentToZip := subdir / 'fileWithContentToZip'.
	fileWithContentToZip ensureCreateFile.
	fileWithContentToZip writeStreamDo: [ :stream | stream nextPutAll: fileContent ].

	zipOutput := zip addTree: subdir relativeTo: (subdir parent) match: [ :e | true ].

	zipOutput extractAllTo: extractionDir.
	extractedFile :=  (extractionDir / (fileWithContentToZip relativeTo: fs root) fullName) asFileReference.
	self assert:	 extractedFile isFile.
	self assert:	 (extractedFile readStreamDo: [ :stream | stream contents]) equals: fileContent
]

{ #category : 'tests' }
ZipArchiveTest >> testAddTreeWithSubDirectoriesShouldSucceed [
	| zipOutput  extractionDir subSubDir fileInSubSubDIr |
	extractionDir := fs root / 'extracted'.
	extractionDir ensureCreateDirectory.
	subSubDir := subdir / 'subSubDir'.
	subSubDir ensureCreateDirectory.
	fileInSubSubDIr := subdir / 'subSubDir' / 'fileInSubSubDir'.
	fileInSubSubDIr ensureCreateFile.

	zipOutput := zip addTree: subdir relativeTo: (subdir parent) match: [ :e | true ].
	zipOutput extractAllTo: extractionDir.

	self assert:	 (extractionDir / 'test-zip-dir' / 'subSubDir') asFileReference isDirectory.
	self assert:	 (extractionDir / 'test-zip-dir' / 'subSubDir' / 'fileInSubSubDir') asFileReference isFile.
	self deny: (extractionDir / 'subSubDir') asFileReference isDirectory
]

{ #category : 'tests - adding' }
ZipArchiveTest >> testArchiveOfAddedFileExtractsNextToIt [
	"As the file browser compresses a file: the archive extracts back to the file alone, without the directories above it"

	| file target |
	file := (fs root / 'home' / 'user') ensureCreateDirectory / 'notes.txt'.
	file writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	zip addFile: file.
	zip writeToFile: zipFile.
	zip close.
	target := (fs root / 'target') ensureCreateDirectory.
	(ZipArchive new readFrom: zipFile)
		extractAllTo: target;
		close.
	self assert: (target children collect: #basename) asArray equals: #( 'notes.txt' ).
	self assert: (target / 'notes.txt') contents equals: 'file contents'
]

{ #category : 'tests' }
ZipArchiveTest >> testArchiveStoresCrc32OfMembers [
	"The CRC-32 of a member is written in the archive and read back."

	| readArchive |
	zip addDeflateString: 'Hello World' as: 'hello.txt'.
	zip writeToFile: zipFile.

	readArchive := ZipArchive new readFrom: zipFile.
	"Standard CRC-32 of 'Hello World', as given by zlib's crc32 or Python's zlib.crc32"
	[ self assert: (readArchive memberNamed: 'hello.txt') crc32 equals: 16r4A17B156 ] ensure: [ readArchive close ]
]

{ #category : 'tests' }
ZipArchiveTest >> testArchiveWithThousandFilesShouldWork [

	"Smoke test showing that we can create zip archives with up to 1000 elements.
	This test was created as a regression test. Previous implementations were leaking files and reaching the open file limits from the operating system."

	| dir theZip |
	dir := (FileLocator temp / 'testfiles') ensureCreateDirectory.
	[
		1 to: 1000 do: [ :i |
			(dir / ('test-' , i asString))
				writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
				yourself ].

		theZip := ZipArchive new.
		dir children do: [ :eachFile | theZip addFile: eachFile resolve ].
		theZip writeToFile: FileLocator temp / 'result.zip'.
		theZip close.
	] ensure: [
		dir ensureDeleteAll.
		(FileLocator temp / 'result.zip') ensureDelete. ]
]

{ #category : 'tests - members' }
ZipArchiveTest >> testByteArrayMembersAreWrittenAsIs [

	| bytes archive |
	bytes := #[ 0 255 128 1 233 ].
	zip addString: bytes as: 'stored.bin'.
	zip addDeflateString: bytes as: 'deflated.bin'.
	archive := self reread: zip.
	self assert: (archive contentsOf: 'stored.bin') equals: bytes.
	self assert: (archive contentsOf: 'deflated.bin') equals: bytes
]

{ #category : 'tests' }
ZipArchiveTest >> testCanUnzipFromFileName [
	| nestedFileToZip subsubdir extracted |
	subsubdir := subdir /'subsubdir'.
	subsubdir ensureCreateDirectory .
	nestedFileToZip := (subdir / '_test-zip-file.txt')
		writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
		yourself.
	zipFile := FileLocator temp / ('pharo_test_ZipArchive_testCanUnzipFromFileName' , '.zip').
	zipFile binaryWriteStreamDo: [ :stream |
		zip
			addDirectory: subdir as: subdir basename;
			addDirectory: subsubdir as: (subsubdir relativeTo: subdir parent) fullName;
			addFile: nestedFileToZip as: nestedFileToZip basename;
			writeTo: stream;
			close. ].

	extracted := fs root / 'extracted'.
	extracted ensureCreateDirectory.
	ZipArchive new
		readFrom: zipFile fullName;
		extractAllTo: extracted overwrite: true;
		close.
	zipFile ensureDelete
]

{ #category : 'tests' }
ZipArchiveTest >> testCanUnzipFromStream [

	| nestedFileToZip extracted |
	nestedFileToZip := (subdir / '_test-zip-file.txt')
		                   writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
		                   yourself.
	zipFile binaryWriteStreamDo: [ :stream |
			zip
				addDirectory: subdir as: subdir basename;
				addFile: nestedFileToZip as: nestedFileToZip basename;
				writeTo: stream;
				close ].

	extracted := (fs root / 'extracted')
		             ensureCreateDirectory;
		             yourself.
	zipFile binaryReadStreamDo: [ :stream |
			ZipArchive new
				readFrom: stream;
				extractAllTo: extracted overwrite: true;
				close ].

	self assert: (extracted / subdir basename) isDirectory.
	self assert: (extracted / nestedFileToZip basename) contents equals: 'file contents'
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testCompressionConstants [

	self assert: ZipArchive compressionStored equals: 0.
	self assert: ZipArchive compressionLevelNone equals: 0.
	self assert: ZipArchive compressionLevelDefault equals: 6
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testContentsOf [

	zip addString: 'hello' as: 'a.txt'.
	self assert: (zip contentsOf: 'a.txt') equals: 'hello'.
	self assert: (zip contentsOf: zip members first) equals: 'hello'.
	self assert: (zip contentsOf: 'missing.txt') isNil
]

{ #category : 'tests - members' }
ZipArchiveTest >> testCrc32StringIsEightHexDigits [

	| bytes members |
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip addDeflateString: '' as: 'empty.txt'.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	members := (ZipArchive new readFrom: bytes readStream) members.
	self assert: members first crc32String equals: '3610A686'.
	self assert: members second crc32String equals: '00000000'
]

{ #category : 'tests' }
ZipArchiveTest >> testCreateWithRelativeNames [
	"Test creating a zip with a relative tree of files, so that the tree will
	be created whereever the ."

	| nestedFileToZip localFileHeaderSignature versionNeededToExtract bitFlag deflateCompressionMethod expectedCrc32 expectedUncompressedSize folderModified centralDirectoryOffset centralDirectoryEnd expectedCompressedSize fileModified fileStart |
	nestedFileToZip := subdir / '_test-zip-file'.
	nestedFileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	fileModified := nestedFileToZip entry modification.
	folderModified := subdir entry modification.
	zip
		addDirectory: subdir
		as: subdir basename.
	zip
		addFile: nestedFileToZip
		as: nestedFileToZip basename.

	zip writeToFile: zipFile.

	zipFile binaryReadStreamDo: [ :str | | endianStream |

		endianStream := ZnEndianessReadWriteStream on: str.

		localFileHeaderSignature := 16r04034b50.
		versionNeededToExtract := 20.
		bitFlag := 0.
		deflateCompressionMethod := 8.
		expectedCrc32 := 16r2ab092ee. "I don''t know how to compute this, but OS X returned the same, so guessing it's correct"
		expectedCompressedSize := 15.
		expectedUncompressedSize := 13.

		"folder"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "No compression".
		self assertLastModificationTimeFrom: endianStream expected: folderModified.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedCrc32".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedCompressedSize".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedUncompressedSize".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: subdir basename size + 1.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: subdir basename size + 1) asString equals: subdir basename, '/'.

		"Test file"
		fileStart := str position.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assertLastModificationTimeFrom: endianStream expected: fileModified.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: nestedFileToZip basename size.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: nestedFileToZip basename size) asString equals: (nestedFileToZip basename copy replaceAll: DiskStore delimiter with: $/).
		self assert: (str next: expectedCompressedSize) isNil not "I don''t understand the compression yet".

		"Central directory structure"
		centralDirectoryOffset := str position.

		"Folder entry"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 16r02014b50.
		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (endianStream nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "no compression".
		self assertLastModificationTimeFrom: endianStream expected: folderModified.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedCrc32".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedCompressedSize".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 0 "expectedUncompressedSize".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: subdir basename size + 1.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".

		self flag: 'I don''t understand the attributes yet'.
		self assert: (endianStream nextLittleEndianNumber: 4) equals:  8r040755 << 16 "External file attributes".
		self assert: (endianStream nextLittleEndianNumber: 4) equals:  0 "Relative offset of local header".
		self assert: (str next: subdir basename size + 1) asString equals: subdir basename, '/'.

		"Nested file entry"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 16r02014b50.

		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (endianStream nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assertLastModificationTimeFrom: endianStream expected: fileModified.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: nestedFileToZip basename size.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".

		self flag: 'I don''t understand the attributes yet'.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 8r0100666 << 16 "External file attributes".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: fileStart "Relative offset of local header".
		self assert: (str next: nestedFileToZip basename size) asString equals:  (nestedFileToZip basename copy replaceAll: DiskStore delimiter with: $/).

		centralDirectoryEnd := str position.

		"End of central directory record"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 16r6054B50 "Signature".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk number".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk # where central dir started".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 2 "Total entries in central dir on this disk".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 2 "Total entries in central dir".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: centralDirectoryEnd  - centralDirectoryOffset "Central directory size".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: centralDirectoryOffset "from start of first disk".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "zip comment length".
		self assert: str atEnd ]
]

{ #category : 'tests' }
ZipArchiveTest >> testDate29Feb2000 [
	"Ensure that dates with leap years survive the DOS timestamp conversion done when writing and reading an archive.
	DOS timestamps only have a 2 seconds resolution, so we use an even number of seconds."

	| theDate readArchive |
	theDate := DateAndTime
		           year: 2000
		           month: 2
		           day: 29
		           hour: 13
		           minute: 23
		           second: 54
		           offset: Duration zero.
	(zip addDeflateString: 'foo' as: 'bar') modifiedAt: theDate.
	zip writeToFile: zipFile.

	readArchive := ZipArchive new readFrom: zipFile.
	[ self assert: (readArchive memberNamed: 'bar') lastModTime equals: theDate ] ensure: [ readArchive close ]
]

{ #category : 'tests - members' }
ZipArchiveTest >> testDesiredCompressionLevelFollowsCompressionMethod [

	| member |
	member := zip addString: 'hello' as: 'a.txt'.
	member desiredCompressionMethod: ZipArchive compressionDeflated.
	self assert: member desiredCompressionLevel equals: ZipArchive compressionLevelDefault.
	member desiredCompressionMethod: ZipArchive compressionStored.
	self assert: member desiredCompressionLevel equals: ZipArchive compressionLevelNone
]

{ #category : 'tests' }
ZipArchiveTest >> testFilePermissions [

	| nestedFileToZip folderMember fileMember |
	nestedFileToZip := subdir / '_test-zip-file'.
	nestedFileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].

	folderMember := zip
		addDirectory: subdir
		as: subdir basename.
	fileMember := zip
		addFile: nestedFileToZip
		as: nestedFileToZip basename.
	zip writeToFile: zipFile.

	self assert: fileMember unixFileAttributes equals: 8r100666.
	self assert: folderMember unixFileAttributes equals: 8r40755
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testHasMemberSuchThat [

	zip addString: 'a' as: 'a.txt'.
	self assert: (zip hasMemberSuchThat: [ :member | member fileName = 'a.txt' ]).
	self deny: (zip hasMemberSuchThat: [ :member | member fileName = 'b.txt' ])
]

{ #category : 'tests - members' }
ZipArchiveTest >> testIsTextFileIsWrittenAndRead [

	| bytes members |
	(zip addDeflateString: 'text' as: 'a.txt') isTextFile: true.
	zip addDeflateString: 'binary' as: 'b.bin'.
	self assert: zip members first isTextFile.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	members := (ZipArchive new readFrom: bytes readStream) members.
	self assert: members first isTextFile.
	self deny: members second isTextFile.
	members first isTextFile: false.
	self deny: members first isTextFile
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testMemberNames [

	zip addString: 'a' as: 'a.txt'.
	zip addString: 'b' as: 'dir/b.txt'.
	self assert: zip memberNames asArray equals: #( 'a.txt' 'dir/b.txt' )
]

{ #category : 'tests' }
ZipArchiveTest >> testParsePharoZipTimestamp [
	"Issue 6054. We're testing a private method. Eventually, the zip timestamp handling should be refactored; maybe a ZipTimestamp subclass of DosTimestamp"

	| dosTimestamp dateAndTime |
	"This was the error-causing timestamp from http://code.google.com/p/pharo/issues/detail?id=6054#c1 . It was the 'package' file inside Balloon-CamilloBruni.85.mcz"
	dosTimestamp := 1001728866.
	dateAndTime := ZipFileMember new unzipTimestamp: dosTimestamp.
	self assert: dateAndTime equals: (DateAndTime fromString: '2011-09-29T04:01:06+00:00')
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testPrependedDataSizeIsZeroWithoutPrependedData [

	| bytes |
	zip addDeflateString: 'hello' as: 'a.txt'.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	self assert: (ZipArchive new readFrom: bytes readStream) prependedDataSize equals: 0
]

{ #category : 'tests' }
ZipArchiveTest >> testReadingDataThatIsNotAnArchiveRaisesZipArchiveError [

	self should: [ ZipArchive new readFrom: #[ 1 2 3 ] readStream ] raise: ZipArchiveError.
	self should: [ ZipArchive new readFrom: (ByteArray new: 100 withAll: 7) readStream ] raise: ZipArchiveError
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testRemoveMember [

	| removed |
	zip addString: 'a' as: 'a.txt'.
	zip addString: 'b' as: 'b.txt'.
	removed := zip removeMember: 'a.txt'.
	self assert: removed fileName equals: 'a.txt'.
	self assert: zip memberNames asArray equals: #( 'b.txt' ).
	self assert: (zip removeMember: 'missing.txt') isNil.
	self assert: zip numberOfMembers equals: 1
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testSetContentsOfAMissingMemberRaisesZipArchiveError [

	self should: [ zip setContentsOf: 'missing.txt' to: 'x' ] raise: ZipArchiveError
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testSetContentsOfReplacesAMemberReadFromAFile [

	| archive |
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip writeToFile: zipFile.
	archive := ZipArchive new readFrom: zipFile.
	[
		archive setContentsOf: 'a.txt' to: 'goodbye'.
		self assert: (archive contentsOf: 'a.txt') equals: 'goodbye'.
		archive writeToFile: fs root / 'rewritten.zip' ] ensure: [ archive close ].
	archive := ZipArchive new readFrom: fs root / 'rewritten.zip'.
	[ self assert: (archive contentsOf: 'a.txt') asString equals: 'goodbye' ] ensure: [ archive close ]
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testSetContentsOfReplacesTheContents [

	| member |
	member := zip addDeflateString: 'hello' as: 'a.txt'.
	member fileComment: 'a comment'.
	member unixFileAttributes: 8r100600.
	zip setContentsOf: 'a.txt' to: 'goodbye'.
	self assert: (zip contentsOf: 'a.txt') equals: 'goodbye'.
	member := zip members first.
	self assert: zip numberOfMembers equals: 1.
	self assert: member fileComment equals: 'a comment'.
	self assert: (member unixFileAttributes bitAnd: 8r777) equals: 8r600.
	self assert: member desiredCompressionMethod equals: ZipArchive compressionDeflated
]

{ #category : 'tests' }
ZipArchiveTest >> testSetLastModification [
	"The modification time survives writing and reading an archive.
	DOS timestamps only have a 2 seconds resolution, so odd seconds are truncated."

	| aDateAndTime readArchive |
	aDateAndTime := DateAndTime
		                year: 2000
		                month: 1
		                day: 15
		                hour: 13
		                minute: 23
		                second: 55
		                offset: Duration zero.
	(zip addDeflateString: 'foo' as: 'bar') modifiedAt: aDateAndTime.
	zip writeToFile: zipFile.

	readArchive := ZipArchive new readFrom: zipFile.
	[ self assert: (readArchive memberNamed: 'bar') lastModTime equals: aDateAndTime - 1 second ] ensure: [ readArchive close ]
]

{ #category : 'tests' }
ZipArchiveTest >> testShouldUnzipAndOverwriteWithoutInforming [
	| nestedFileToZip fileModificationTime fileModificationTimeBlock |
	fileModificationTimeBlock := [ (subdir / '_test-zip-file.txt') modificationTime ].
	nestedFileToZip := (subdir / '_test-zip-file.txt')
		writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
		yourself.
	zip
		addDirectory: subdir as: subdir basename;
		addFile: nestedFileToZip as: nestedFileToZip basename;
		writeToFile: zipFile;
		close.
	ZipArchive new
		readFrom: zipFile;
		extractAllTo: subdir overwrite: true;
		close.
	fileModificationTime := fileModificationTimeBlock value.
	1 second wait.
	ZipArchive new
		readFrom: zipFile;
		extractAllTo: subdir overwrite: true;
		close.
	self assert: fileModificationTime ~= fileModificationTimeBlock value.
	fileModificationTime := fileModificationTimeBlock value.
	1 second wait.
	ZipArchive new
		readFrom: zipFile;
		extractAllTo: subdir overwrite: false;
		close.
	self deny: fileModificationTime ~= fileModificationTimeBlock value
]

{ #category : 'tests' }
ZipArchiveTest >> testStoredMemberCanBeCopiedToAnotherArchive [
	"Writing an archive that was read copies its stored members, which also checks their CRC-32."

	| readArchive copyFile copiedArchive |
	zip addString: 'Hello World' as: 'hello.txt'.
	zip writeToFile: zipFile.
	copyFile := fs root / 'copy.zip'.

	readArchive := ZipArchive new readFrom: zipFile.
	[ readArchive writeToFile: copyFile ] ensure: [ readArchive close ].

	copiedArchive := ZipArchive new readFrom: copyFile.
	[ self assert: (copiedArchive memberNamed: 'hello.txt') contents asString equals: 'Hello World' ] ensure: [ copiedArchive close ]
]

{ #category : 'tests' }
ZipArchiveTest >> testStoredMemberWithCorrectCrc32IsRead [

	| readArchive member |
	zip addString: 'Hello World' as: 'hello.txt'.
	zip writeToFile: zipFile.

	readArchive := ZipArchive new readFrom: zipFile.
	[
		member := readArchive memberNamed: 'hello.txt'.
		self assert: member compressionMethod equals: 0. "CompressionStored"
		self assert: member contents asString equals: 'Hello World'.
		self deny: member isCorrupt ] ensure: [ readArchive close ]
]

{ #category : 'tests - members' }
ZipArchiveTest >> testStoredStringMemberIsWrittenToAByteArrayStream [

	zip addString: 'hello' as: 'a.txt'.
	self assert: ((self reread: zip) contentsOf: 'a.txt') equals: 'hello' asByteArray
]

{ #category : 'tests - members' }
ZipArchiveTest >> testStringMembersAreWrittenInUTF8 [

	| text archive |
	text := 'été ' , (String with: (Character value: 16r4E2D)).
	zip addString: text as: 'stored.txt'.
	zip addDeflateString: text as: 'deflated.txt'.
	self assert: zip members first uncompressedSize equals: text utf8Encoded size.
	archive := self reread: zip.
	self assert: (archive contentsOf: 'stored.txt') equals: text utf8Encoded.
	self assert: (archive contentsOf: 'deflated.txt') equals: text utf8Encoded.
	self assert: (archive contentsOf: 'deflated.txt') utf8Decoded equals: text
]

{ #category : 'tests - accessing' }
ZipArchiveTest >> testSymbolicLinkHandler [

	| handler |
	self assert: zip symbolicLinkHandler isNil.
	handler := [ :member :file |  ].
	zip symbolicLinkHandler: handler.
	self assert: zip symbolicLinkHandler identicalTo: handler
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testWriteToFileNeededByAMemberRaisesZipArchiveError [

	zip addDeflateString: 'hello' as: 'a.txt'.
	zip writeToFile: zipFile.
	zip := ZipArchive new readFrom: zipFile.
	[ self should: [ zip writeToFile: zipFile ] raise: ZipArchiveError ] ensure: [ zip close ].
	self assert: ((ZipArchive new readFrom: zipFile) contentsOf: 'a.txt') asString equals: 'hello'
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testWriteToFileReplacesALongerFile [

	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: (ByteArray new: 5000 withAll: 7) ].
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip writeToFile: zipFile.
	self assert: zipFile size < 5000.
	self assert: ((ZipArchive new readFrom: zipFile) contentsOf: 'a.txt') asString equals: 'hello'
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testWriteToPrependingFile [

	| prefix bytes archive |
	prefix := fs root / 'prefix.bin'.
	prefix binaryWriteStreamDo: [ :stream | stream nextPutAll: #[ 1 2 3 4 5 6 7 ] ].
	zip addDeflateString: 'hello' as: 'a.txt'.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream prependingFile: prefix ].
	self assert: (bytes first: 7) equals: #[ 1 2 3 4 5 6 7 ].
	archive := ZipArchive new readFrom: bytes readStream.
	self assert: archive prependedDataSize equals: 7.
	self assert: (archive contentsOf: 'a.txt') asString equals: 'hello'
]

{ #category : 'tests' }
ZipArchiveTest >> testWrongCrc32InDeflatedMemberRaisesCRCError [
	"Change the CRC-32 stored in the archive (local header and central directory) and check that reading the member detects it."

	| crcBytes bytes corruptedBytes readArchive |
	zip addDeflateString: 'Hello World' as: 'hello.txt'.
	zip writeToFile: zipFile.

	"Little endian bytes of 16r4A17B156, the CRC-32 of 'Hello World'"
	crcBytes := #[ 16r56 16rB1 16r17 16r4A ].
	bytes := zipFile binaryReadStreamDo: [ :stream | stream upToEnd ].
	corruptedBytes := bytes copyReplaceAll: crcBytes with: #[ 16r57 16rB1 16r17 16r4A ].
	self deny: corruptedBytes equals: bytes.
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: corruptedBytes ].

	readArchive := ZipArchive new readFrom: zipFile.
	[ self should: [ (readArchive memberNamed: 'hello.txt') contents ] raise: CRCError ] ensure: [ readArchive close ]
]

{ #category : 'tests' }
ZipArchiveTest >> testWrongCrc32InStoredMemberRaisesCRCError [
	"Same as #testWrongCrc32InDeflatedMemberRaisesCRCError but for a stored (not compressed) member."

	| crcBytes bytes corruptedBytes readArchive |
	zip addString: 'Hello World' as: 'hello.txt'.
	zip writeToFile: zipFile.

	"Little endian bytes of 16r4A17B156, the CRC-32 of 'Hello World'"
	crcBytes := #[ 16r56 16rB1 16r17 16r4A ].
	bytes := zipFile binaryReadStreamDo: [ :stream | stream upToEnd ].
	corruptedBytes := bytes copyReplaceAll: crcBytes with: #[ 16r57 16rB1 16r17 16r4A ].
	self deny: corruptedBytes equals: bytes.
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: corruptedBytes ].

	readArchive := ZipArchive new readFrom: zipFile.
	[
		self assert: (readArchive memberNamed: 'hello.txt') compressionMethod equals: 0. "CompressionStored"
		self should: [ (readArchive memberNamed: 'hello.txt') contents ] raise: CRCError.
		self assert: (readArchive memberNamed: 'hello.txt') isCorrupt ] ensure: [ readArchive close ]
]

{ #category : 'tests' }
ZipArchiveTest >> testZip [

	| localFileHeaderSignature versionNeededToExtract bitFlag deflateCompressionMethod realModificationStamp expectedCrc32 expectedCompressedSize expectedUncompressedSize centralDirectoryOffset centralDirectoryEnd |

	realModificationStamp := fileToZip entry modification.
	zip
		addFile: fileToZip
		as: fileToZip basename.

	zip writeToFile: zipFile.

	zipFile binaryReadStreamDo: [ :str | | endianStream |

		endianStream := ZnEndianessReadWriteStream on: str.

		localFileHeaderSignature := 16r04034b50.
		versionNeededToExtract := 20.
		bitFlag := 0.
		deflateCompressionMethod := 8.
		expectedCrc32 := 16r2ab092ee. "I don''t know how to compute this, but OS X returned the same, so guessing it's correct"
		expectedCompressedSize := 15.
		expectedUncompressedSize := 13.

		"Test file"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assertLastModificationTimeFrom: endianStream expected: realModificationStamp.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: fileToZip basename size.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: fileToZip basename size) asString equals: fileToZip basename.
		self assert: (str next: expectedCompressedSize) isNil not "I don''t understand the compression yet".

		"Central directory structure"
		centralDirectoryOffset := str position.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 16r02014b50.

		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (endianStream nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assertLastModificationTimeFrom: endianStream expected: realModificationStamp.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: fileToZip basename size.
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".

		self flag: 'I don''t understand the attributes yet'.
		self assert: (endianStream nextLittleEndianNumber: 4) equals:  16r81B60000 "External file attributes".
		self assert: (endianStream nextLittleEndianNumber: 4) equals:  0 "Relative offset of local header".
		self assert: (str next: fileToZip basename size) asString equals: fileToZip basename.
		centralDirectoryEnd := str position.

		"End of central directory record"
		self assert: (endianStream nextLittleEndianNumber: 4) equals: 16r6054B50 "Signature".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk number".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "Disk # where central dir started".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 1 "Total entries in central dir on this disk".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 1 "Total entries in central dir".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: centralDirectoryEnd  - centralDirectoryOffset "Central directory size".
		self assert: (endianStream nextLittleEndianNumber: 4) equals: centralDirectoryOffset "from start of first disk".
		self assert: (endianStream nextLittleEndianNumber: 2) equals: 0 "zip comment length".
		self assert: str atEnd ]
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testZipFileCommentInCp437IsDecoded [
	"Without UTF-8, the comment is read as CP437, in which 16r82 is é"

	| bytes |
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip zipFileComment: 'xx'.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	bytes at: bytes size - 1 put: 16r82.
	bytes at: bytes size put: $A asInteger.
	self assert: (ZipArchive new readFrom: bytes readStream) zipFileComment equals: 'éA'
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testZipFileCommentIsWrittenAndRead [

	| bytes |
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip zipFileComment: 'an archive comment'.
	zip writeToFile: zipFile.
	self assert: (ZipArchive new readFrom: zipFile) zipFileComment equals: 'an archive comment'.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	self assert: (ZipArchive new readFrom: bytes readStream) zipFileComment equals: 'an archive comment'
]

{ #category : 'tests - writing' }
ZipArchiveTest >> testZipFileCommentIsWrittenInUTF8 [

	| comment bytes |
	comment := 'été ' , (String with: (Character value: 16r4E2D)).
	zip addDeflateString: 'hello' as: 'a.txt'.
	zip zipFileComment: comment.
	bytes := ByteArray streamContents: [ :stream | zip writeTo: stream ].
	self assert: (bytes last: comment utf8Encoded size) equals: comment utf8Encoded.
	self assert: (ZipArchive new readFrom: bytes readStream) zipFileComment equals: comment
]

{ #category : 'tests' }
ZipArchiveTest >> testisZipArchive [
	| nonArchive |
	zip addFile: fileToZip as: fileToZip basename.
	zip writeToFile: zipFile.
	self assert: (ZipArchive isZipArchive: zipFile).
	self assert: (zipFile binaryReadStreamDo: [ :s | ZipArchive isZipArchive: s ]).
	nonArchive := 'non_archive_file_for_test.txt' asFileReference.
	nonArchive ensureCreateFile.
	[ self deny: (ZipArchive isZipArchive: nonArchive).
	self deny: (nonArchive binaryReadStreamDo: [ :s | ZipArchive isZipArchive: s ]) ]
		ensure: [ nonArchive ensureDelete ]
]
