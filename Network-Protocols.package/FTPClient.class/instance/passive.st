protocol
passive
	self sendCommand: 'PASV'.
	self lookForCode: 227