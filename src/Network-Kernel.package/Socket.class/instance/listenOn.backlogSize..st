connection open/close
listenOn: portNumber backlogSize: backlog
	"Listen for a connection on the given port.
	If this method succeeds, #accept may be used to establish a new connection"
	| status |
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before listening for a new connection'].
	self primSocket: socketHandle listenOn: portNumber backlogSize: backlog.
