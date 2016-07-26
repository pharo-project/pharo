protocol
getDirectory
	| dirList |
	self openPassiveDataConnection.
	self sendCommand: 'LIST'.
	dirList := self getData.
	self checkResponse.
	self checkResponse.
	^dirList
