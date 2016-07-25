protocol
getFileList
	| dirList |
	self openPassiveDataConnection.
	self sendCommand: 'NLST'.
	dirList := self getData.
	self checkResponse.
	self checkResponse.
	^dirList
