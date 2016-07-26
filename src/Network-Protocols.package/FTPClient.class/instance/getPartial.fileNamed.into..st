protocol
getPartial: limit fileNamed: remoteFileName into: dataStream
	| data |
	self openPassiveDataConnection.
	self sendCommand: 'RETR ', remoteFileName.
	[self checkResponse]
		on: TelnetProtocolError
		do: [:ex |
			self closeDataSocket.
			ex pass].
	data := self get: limit dataInto: dataStream.
	self abortDataConnection.
	^data
