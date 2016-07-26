private protocol
initiateSession
	"EHLO <SP> <domain> <CRLF>"

	self sendCommand: (self useHelo ifTrue:['HELO '] ifFalse: ['EHLO ']) , NetNameResolver localHostName.
	self checkResponse.
