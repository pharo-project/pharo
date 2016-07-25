options
route
	"Return the route of the server.
	This is a short identification string to be appended at the end of server session ids, separated by a dot.
	Routes are used by load balancers and proxies to correctly implement session affiinity or stickyness.
	The default is nil, meaning that no route has to be appended."
	
	^ self optionAt: #route ifAbsent: [ nil ]