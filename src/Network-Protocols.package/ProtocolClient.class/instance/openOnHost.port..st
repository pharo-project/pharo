private
openOnHost: hostIP port: portNumber
	self host: hostIP.
	self port: portNumber.
	self ensureConnection