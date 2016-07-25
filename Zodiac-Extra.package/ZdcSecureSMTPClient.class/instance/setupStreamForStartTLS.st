private
setupStreamForStartTLS
	"See http://www.ietf.org/rfc/rfc3207.txt for how to setup a secure connection"

	self stream: (SocketStream openConnectionToHost: self host port: self port).
	self checkResponse.
	self initiateSession.
	self requireStartTLS.
	self startTLS