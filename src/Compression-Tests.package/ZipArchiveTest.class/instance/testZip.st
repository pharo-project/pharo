tests
testZip
	
	| localFileHeaderSignature versionNeededToExtract bitFlag deflateCompressionMethod realModificationStamp expectedCrc32 expectedCompressedSize expectedUncompressedSize centralDirectoryOffset centralDirectoryEnd |
	
	realModificationStamp := fileToZip entry modification.
	zip	
		addFile: fileToZip fullName
		as: fileToZip basename.
		
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
		
		"Test file"
		self assert: (str nextLittleEndianNumber: 4) equals: localFileHeaderSignature.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: realModificationStamp - 1 second and: realModificationStamp + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (str nextLittleEndianNumber: 2) equals: fileToZip basename size.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str next: fileToZip basename size) asString equals: fileToZip basename.
		self assert: (str next: expectedCompressedSize) isNil not "I don''t understand the compression yet".
		
		"Central directory structure"
		centralDirectoryOffset := str position.
		self assert: (str nextLittleEndianNumber: 4) equals: 16r02014b50.
		
		self flag: 'I think this is wrong. What is version 1.4?!'.
		self assert: (str nextLittleEndianNumber: 2) equals:  16r314.
		self assert: (str nextLittleEndianNumber: 2) equals: versionNeededToExtract.
		self assert: (str nextLittleEndianNumber: 2) equals: bitFlag.
		self assert: (str nextLittleEndianNumber: 2) equals: deflateCompressionMethod.
		self assert: ((DateAndTime fromDosTimestamp: (str nextLittleEndianNumber: 4)) between: realModificationStamp - 1 second and: realModificationStamp + 1 seconds).
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCrc32.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedCompressedSize.
		self assert: (str nextLittleEndianNumber: 4) equals: expectedUncompressedSize.
		self assert: (str nextLittleEndianNumber: 2) equals: fileToZip basename size.
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Extra header length".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "File comment".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk number start".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Internal file attributes".
		
		self flag: 'I don''t understand the attributes yet'.
		self assert: (str nextLittleEndianNumber: 4) equals:  16r81B60000 "External file attributes".
		self assert: (str nextLittleEndianNumber: 4) equals:  0 "Relative offset of local header".
		self assert: (str next: fileToZip basename size) asString equals: fileToZip basename.
		centralDirectoryEnd := str position.
		
		"End of central directory record"
		self assert: (str nextLittleEndianNumber: 4) equals: 16r6054B50 "Signature".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk number".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "Disk # where central dir started".
		self assert: (str nextLittleEndianNumber: 2) equals: 1 "Total entries in central dir on this disk".
		self assert: (str nextLittleEndianNumber: 2) equals: 1 "Total entries in central dir".
		self assert: (str nextLittleEndianNumber: 4) equals: centralDirectoryEnd  - centralDirectoryOffset "Central directory size".
		self assert: (str nextLittleEndianNumber: 4) equals: centralDirectoryOffset "from start of first disk".
		self assert: (str nextLittleEndianNumber: 2) equals: 0 "zip comment length".
		self assert: str atEnd ].