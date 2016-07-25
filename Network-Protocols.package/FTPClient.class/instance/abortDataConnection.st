protocol
abortDataConnection
	self sendCommand: 'ABOR'.
	self closeDataSocket