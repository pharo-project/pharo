protocol
ascii
	self sendCommand: 'TYPE A'.
	self lookForCode: 200