connection open/close
closeAndDestroy: timeoutSeconds
	"First, try to close this connection gracefully. If the close attempt fails or times out, abort the connection. In either case, destroy the socket. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	socketHandle ifNotNil: [
			self isConnected ifTrue: [
				self close.  "close this end"
				(self waitForDisconnectionFor: timeoutSeconds) ifFalse: [
						"The other end didn't close so we just abort the connection"
						self primSocketAbortConnection: socketHandle]].
			self destroy].
