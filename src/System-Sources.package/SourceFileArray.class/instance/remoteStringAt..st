public - string reading
remoteStringAt: sourcePointer

	| filePosition fileIndex |
	fileIndex := self fileIndexFromSourcePointer: sourcePointer.
	filePosition := self filePositionFromSourcePointer: sourcePointer.	

	^ RemoteString newFileNumber: fileIndex position: filePosition