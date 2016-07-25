private
writeEndOfCentralDirectoryTo: aStream

	aStream nextPutAll: EndOfCentralDirectorySignature.
	aStream nextLittleEndianNumber: 2 put: 0. "diskNumber"
	aStream nextLittleEndianNumber: 2 put: 0. "diskNumberWithStartOfCentralDirectory"
	aStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectoriesOnThisDisk"
	aStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectories"
	aStream nextLittleEndianNumber: 4 put: writeEOCDOffset - writeCentralDirectoryOffset. "size of central dir"
	aStream nextLittleEndianNumber: 4 put: writeCentralDirectoryOffset. "offset of central dir"
	aStream nextLittleEndianNumber: 2 put: zipFileComment size. "zip file comment"
	zipFileComment isEmpty ifFalse: [ aStream nextPutAll: zipFileComment ].

