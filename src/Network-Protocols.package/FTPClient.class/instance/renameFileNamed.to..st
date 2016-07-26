protocol
renameFileNamed: oldFileName to: newFileName
	self sendCommand: 'RNFR ' , oldFileName.
	self lookForCode: 350.
	self sendCommand: 'RNTO ' , newFileName.
	self lookForCode: 250