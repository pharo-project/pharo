private protocol
recipient: aRecipient
	"specify a recipient for the message.  aRecipient should be a bare email address"
	"RCPT <SP> TO:<forward-path> <CRLF>"

	self sendCommand: 'RCPT TO: <', aRecipient, '>'.
	self checkResponse.