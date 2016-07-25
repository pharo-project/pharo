protocol
makeDirectory: newDirName
	self sendCommand: 'MKD ' , newDirName.
	self checkResponse.
