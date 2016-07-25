protocol
deleteDirectory: dirName
	self sendCommand: 'RMD ' , dirName.
	self checkResponse.
