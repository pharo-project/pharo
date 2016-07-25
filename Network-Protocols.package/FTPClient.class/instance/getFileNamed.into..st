protocol
getFileNamed: remoteFileName into: dataStream
	self openPassiveDataConnection.
	self sendCommand: 'RETR ', remoteFileName.
	[self checkResponse]
		on: TelnetProtocolError
		do: [:ex |
			self closeDataSocket.
			ex pass].
	self getDataInto: dataStream.
	self closeDataSocket.
	self checkResponse