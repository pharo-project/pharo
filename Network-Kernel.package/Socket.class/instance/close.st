connection open/close
close
	"Close this connection gracefully. For TCP, this sends a close request, but the stream remains open until the other side also closes it."

	self primSocketCloseConnection: socketHandle.  "close this end"
