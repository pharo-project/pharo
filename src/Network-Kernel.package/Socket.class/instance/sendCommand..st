sending
sendCommand: commandString
	"Send the given command as a single line followed by a <CR><LF> terminator."

	self sendData: commandString, String crlf.