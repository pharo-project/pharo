private-extensions-sourcefilearray
remoteStringAt: sourcePointer
	<flExtensionOf: #SourceFileArray>
	| filePosition fileIndex |
	fileIndex := self fileIndexFromSourcePointer: sourcePointer.
	filePosition := self filePositionFromSourcePointer: sourcePointer.	

	^ RemoteString newFileNumber: fileIndex position: filePosition