private protocol
quit
	"send a QUIT command.  This is polite to do, and indeed some servers might drop messages that don't have an associated QUIT"
	"QUIT <CRLF>"

	self sendCommand: 'QUIT'.
	self checkResponse.