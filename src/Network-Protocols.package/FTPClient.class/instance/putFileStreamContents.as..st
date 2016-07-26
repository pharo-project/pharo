protocol
putFileStreamContents: fileStream as: fileNameOnServer
	"FTP a file to the server."


	self openPassiveDataConnection.
	self sendCommand: 'STOR ', fileNameOnServer.

	fileStream reset.

	[self sendStreamContents: fileStream]
		ensure: [self closeDataSocket].

	self checkResponse.
	self checkResponse.
