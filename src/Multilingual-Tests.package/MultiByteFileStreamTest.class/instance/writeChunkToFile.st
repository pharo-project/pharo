support
writeChunkToFile
	(File openAndTruncateForWriteFileNamed: 'chunkTest.txt') 
		nextPutAll: self chunkString;
		close