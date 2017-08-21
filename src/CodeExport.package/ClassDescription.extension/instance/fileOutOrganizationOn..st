*CodeExport
fileOutOrganizationOn: aFileStream
	"File a description of the receiver's organization on aFileStream."

	aFileStream cr; nextPut: $!.
	aFileStream nextChunkPut: self name, ' reorganize'; cr.
	aFileStream nextChunkPut: self organization stringForFileOut ; cr