example
finger: userName
	"SocketStream finger: 'stp'"

	| addr s |
	addr := NetNameResolver promptUserForHostAddress.
	s := SocketStream openConnectionToHost: addr port: 79.  "finger port number"
	Transcript show: '---------- Connecting ----------'; cr.
	s sendCommand: userName.
	Transcript show: s nextLineCrLf.
	s close.
	Transcript show: '---------- Connection Closed ----------'; cr; endEntry.
