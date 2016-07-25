private protocol
startTLS
	"Send a STARTTLS command and after the response, 
	switch to a SecureSocketStream and do the client handshake"
	
	self sendCommand: 'STARTTLS'.
	self checkResponse.	
	self stream: (ZdcSecureSocketStream on: self stream socket).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'