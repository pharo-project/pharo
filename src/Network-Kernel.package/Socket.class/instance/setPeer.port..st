datagrams
setPeer: hostAddress port: port
	"Set the default send/recv address."

	self primSocket: socketHandle connectTo: hostAddress port: port.
