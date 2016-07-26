connection open/close
connectTo: hostAddress port: port
	"Initiate a connection to the given port at the given host address.
	Waits until the connection is established or time outs."

	self connectTo: hostAddress port: port waitForConnectionFor: Socket standardTimeout