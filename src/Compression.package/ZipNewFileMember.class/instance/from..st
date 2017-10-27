initialization
from: aFileName

	| entry |
	"Now get the size, attributes, and timestamps, and see if the file exists"
	stream := File openForReadFileNamed: aFileName.
	self localFileName: (externalFileName := stream name).
	entry := stream directoryEntry.
	compressedSize := uncompressedSize := entry size.
	desiredCompressionMethod := compressedSize > 0 ifTrue: [ CompressionDeflated ] ifFalse: [ CompressionStored ].
	self flag: 'When we replace Files with FileSystem, the following line won''t have to jump throught hoops (FS returns aDateAndTime)'.
	self modifiedAt: entry modificationTime.
