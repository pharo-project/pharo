"
SUnit tests for ZIP archives
"
Class {
	#name : #ZipArchiveTest,
	#superclass : #TestCase,
	#instVars : [
		'fileToZip',
		'zip',
		'subdir',
		'zipFile',
		'fs'
	],
	#category : #'Compression-Tests-Archives'
}

{ #category : #tests }
ZipArchiveTest >> assertLastModificationTimeFrom: endianStream expected: fileModified [
	| lastModificationTime |
	lastModificationTime := DateAndTime fromDosTimestamp: (endianStream nextLittleEndianNumber: 4).
	self assert:
			(lastModificationTime
				between: fileModified - 2 seconds
				and: fileModified + 2 seconds).
]

{ #category : #running }
ZipArchiveTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	fileToZip := fs root / 'test-zip-file'.
	fileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	subdir := fs root / 'test-zip-dir'.
	subdir ensureCreateDirectory.
	zipFile := fs root / ('pharo_test' , '.zip').
	zip := ZipArchive new.
]

{ #category : #running }
ZipArchiveTest >> tearDown [
	"Must close the zipfile before deleting all files.
	Otherwise in windows the file will not be deleted as long as the file is open."

	[ zip close ]
		ensure: [ fileToZip ensureDelete.
			subdir ensureDeleteAll.
			zipFile ensureDelete ].
	super tearDown
]

{ #category : #tests }
ZipArchiveTest >> testAddNonExistentFile [
	
	self should: [ zip addFile: 'it_would_be_crazy_if_this_file_existed.ext' asFileReference ] raise: FileDoesNotExistException.
]

{ #category : #tests }
ZipArchiveTest >> testCanUnzipFromFileName [
	| nestedFileToZip |
	nestedFileToZip := (subdir / '_test-zip-file.txt')
		writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
		yourself.
	zipFile := FileLocator temp / ('pharo_test_ZipArchive_testCanUnzipFromFileName' , '.zip').
	zipFile binaryWriteStreamDo: [ :stream |
		zip
			addDirectory: subdir as: subdir basename;
			addFile: nestedFileToZip as: nestedFileToZip basename;
			writeTo: stream;
			close. ].
	
	ZipArchive new
		readFrom: zipFile fullName;
		extractAllTo: subdir overwrite: true;
		close.
	zipFile ensureDelete.

]

{ #category : #tests }
ZipArchiveTest >> testCanUnzipFromStream [
	| nestedFileToZip |
	nestedFileToZip := (subdir / '_test-zip-file.txt')
		writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ];
		yourself.
	zipFile binaryWriteStreamDo: [ :stream |
		zip
			addDirectory: subdir as: subdir basename;
			addFile: nestedFileToZip as: nestedFileToZip basename;
			writeTo: stream;
			close. ].
	
	zipFile binaryReadStreamDo: [ :stream |
		ZipArchive new
			readFrom: stream ;
			extractAllTo: subdir overwrite: true;
			close ].

]

{ #category : #tests }
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
		self assert: str atEnd ].
]

{ #category : #tests }
ZipArchiveTest >> testDate29Feb2000 [
	"Ensure that dates with leap years don't screw up in the conversion"

	| archive member theDate |
	theDate := Date year: 2000 month: 2 day: 29.
	archive := ZipArchive new.
	member := archive addDeflateString: 'foo' as: 'bar'.
	member modifiedAt: theDate.
	self assert: member lastModTime equals: theDate.
]

{ #category : #tests }
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
	self assert: folderMember unixFileAttributes equals: 8r40755.
]

{ #category : #tests }
ZipArchiveTest >> testParsePharoZipTimestamp [
	"Issue 6054. We're testing a private method. Eventually, the zip timestamp handling should be refactored; maybe a ZipTimestamp subclass of DosTimestamp"

	| dosTimestamp dateAndTime |
	"This was the error-causing timestamp from http://code.google.com/p/pharo/issues/detail?id=6054#c1 . It was the 'package' file inside Balloon-CamilloBruni.85.mcz"
	dosTimestamp := 1001728866.
	dateAndTime := ZipFileMember new unzipTimestamp: dosTimestamp.
	self assert: dateAndTime equals: '2011-09-29T04:01:06+00:00' asDateAndTime.
]

{ #category : #tests }
ZipArchiveTest >> testSetLastModification [

	| aDateAndTime member |
	aDateAndTime := '15 January, 2000T13:23:55'.
	member := zip addDeflateString: 'foo' as: 'bar'.
	member modifiedAt: aDateAndTime.
	self assert: member lastModTime equals: aDateAndTime.
]

{ #category : #tests }
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

{ #category : #tests }
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
		self assert: str atEnd ].
]

{ #category : #tests }
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
