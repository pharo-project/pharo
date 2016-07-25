primitives
primSocket: socketID getOption: aString 
	"Get some option information on this socket. Refer to the UNIX 
	man pages for valid SO, TCP, IP, UDP options. In case of doubt
	refer to the source code.
	TCP:=NODELAY, SO:=KEEPALIVE are valid options for example
	returns an array containing the error code and the option value"

	<primitive: 'primitiveSocketGetOptions' module: 'SocketPlugin'>
	self primitiveFailed
