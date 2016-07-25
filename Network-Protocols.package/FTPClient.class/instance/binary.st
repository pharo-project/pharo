protocol
binary
	self sendCommand: 'TYPE I'.
	self lookForCode: 200