tests
testCreateWithRelativeNames
	"Test creating a zip with a relative tree of files, so that the tree will
	be created whereever the ."
	
	| nestedFileToZip localFileHeaderSignature versionNeededToExtract bitFlag deflateCompressionMethod expectedCrc32 expectedUncompressedSize folderModified centralDirectoryOffset centralDirectoryEnd expectedCompressedSize fileModified fileStart |
	nestedFileToZip := subdir / '_test-zip-file'.
	nestedFileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	fileModified := nestedFileToZip entry modification.
	folderModified := subdir entry modification.
	zip
		addDirectory: subdir fullName
		as: subdir basename.
	zip	
		addFile: nestedFileToZip fullName
		as: nestedFileToZip basename.
		
	zip writeToFileNamed: zipFile fullName.
	
	zipFile readStreamDo: [ :str |
		str binary.
		localFileHeaderSignature := 16r04034b50.
		versionNeededToExtract := 20.
		bitFlag := 0.
		deflateCompressionMethod := 8.
		expectedCrc32 := 16r2ab092ee. "I don''t know how to compute this, but OS X returned the same, so guessing it's correct"
		expectedCompressedSize := 15.
		expectedUncompressedSize := 13.
		
		"folder"
		self assert: (str nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "No compression".
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: folderModified - 1 second and: folderModified + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedCrc32".
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedCompressedSize".
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedUncompressedSize".
		self assert: (str nextLittleEndianNumber: 2) equals: subdir basename size + 1.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: subdir basename size + 1) asString equals: subdir basename, '/'.
		
		"Test file"
		fileStart := str position.
		self assert: (str nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: fileModified - 1 second and: fileModified + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (str nextLittleEndianNumber: 2) equals: nestedFileToZip basename size.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: nestedFileToZip basename size) asString equals: (nestedFileToZip basename copy replaceAll: DiskStore delimiter with: $/).
		self assert: (str next: expectedCompressedSize) isNil not "I don''t understand the compression yet".
		
		"Central directory structure"
		centralDirectoryOffset := str position.
		
		"Folder entry"
		self assert: (str nextLittleEndianNumber: 4) equals: 16r02014b50.	
		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (str nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "no compression".
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: folderModified - 1 second and: folderModified + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedCrc32".
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedCompressedSize".
		self assert: (str nextLittleEndianNumber: 4) equals: 0 "expectedUncompressedSize".
		self assert: (str nextLittleEndianNumber: 2) equals: subdir basename size + 1.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".
		
		self flag: 'I don''t understand the attributes yet'.
		self assert: (str nextLittleEndianNumber: 4) equals:  8r040755 << 16 "External file attributes".
		self assert: (str nextLittleEndianNumber: 4) equals:  0 "Relative offset of local header".
		self assert: (str next: subdir basename size + 1) asString equals: subdir basename, '/'.
		
		"Nested file entry"
		self assert: (str nextLittleEndianNumber: 4) equals: 16r02014b50.
		
		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (str nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: fileModified - 1 second and: fileModified + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (str nextLittleEndianNumber: 2) equals: nestedFileToZip basename size.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".
		
		self flag: 'I don''t understand the attributes yet'.
		self assert: (str nextLittleEndianNumber: 4) equals: 8r0100666 << 16 "External file attributes".
		self assert: (str nextLittleEndianNumber: 4) equals: fileStart "Relative offset of local header".
		self assert: (str next: nestedFileToZip basename size) asString equals:  (nestedFileToZip basename copy replaceAll: DiskStore delimiter with: $/).

		centralDirectoryEnd := str position.
		
		"End of central directory record"
		self assert: (str nextLittleEndianNumber: 4) equals: 16r6054B50 "Signature".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk number".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk # where central dir started".
		self assert: (str nextLittleEndianNumber: 2) equals: 2 "Total entries in central dir on this disk".
		self assert: (str nextLittleEndianNumber: 2) equals: 2 "Total entries in central dir".
		self assert: (str nextLittleEndianNumber: 4) equals: centralDirectoryEnd  - centralDirectoryOffset "Central directory size".
		self assert: (str nextLittleEndianNumber: 4) equals: centralDirectoryOffset "from start of first disk".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "zip comment length".
		self assert: str atEnd ].