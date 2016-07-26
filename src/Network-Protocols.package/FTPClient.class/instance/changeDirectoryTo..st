protocol
changeDirectoryTo: newDirName
	self sendCommand: 'CWD ' , newDirName.
	self checkResponse.
