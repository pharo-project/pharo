protocol
deleteFileNamed: fileName
	self sendCommand: 'DELE ' , fileName.
	self checkResponse.
