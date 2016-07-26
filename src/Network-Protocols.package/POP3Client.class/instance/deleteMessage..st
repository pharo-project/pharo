public protocol
deleteMessage: num
	"delete the numbered message"

	self ensureConnection.
	self sendCommand: 'DELE ', num printString.
	self checkResponse.
	self logProgress: self lastResponse