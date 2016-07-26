public protocol
retrieveMessage: number
	"retrieve the numbered message"

	self ensureConnection.
	self sendCommand: 'RETR ', number printString.
	self checkResponse.
	self logProgress: self lastResponse.

	^self getMultilineResponse