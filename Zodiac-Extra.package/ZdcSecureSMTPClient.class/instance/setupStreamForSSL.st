private
setupStreamForSSL
	self stream: (ZdcSecureSocketStream openConnectionToHost: self host port: self port).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'.
	"as we connect directly with SSL the first message will be a 220 server greeting"
	self checkResponse.
