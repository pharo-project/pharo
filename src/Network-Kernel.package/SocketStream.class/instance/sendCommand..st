stream out
sendCommand: aString
	"Sends a String ending it with CR LF and then flush
	causing it to block until sent."

	self nextPutAll: aString, String crlf; flush