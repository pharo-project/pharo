connection open/close
accept
	"Accept a connection from the receiver socket.
	Return a new socket that is connected to the client"
	^Socket acceptFrom: self.