private
setupStreamForSSL
	self stream: (ZdcSecureSocketStream openConnectionToHost: self host port: self port).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'