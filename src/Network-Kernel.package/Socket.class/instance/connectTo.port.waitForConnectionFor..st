connection open/close
connectTo: hostAddress port: port waitForConnectionFor: timeout 
	"Initiate a connection to the given port at the given host 
	address. Waits until the connection is established or time outs."
	self connectNonBlockingTo: hostAddress port: port.
	self
		waitForConnectionFor: timeout
		ifTimedOut: [ConnectionTimedOut signal: 'Cannot connect to '
					, (NetNameResolver stringFromAddress: hostAddress) , ':' , port asString]