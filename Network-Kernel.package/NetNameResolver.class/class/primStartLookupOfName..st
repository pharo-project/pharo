primitives
primStartLookupOfName: hostName
	"Look up the given host name in the Domain Name Server to find its address. This call is asynchronous. To get the results, wait for it to complete or time out and then use primNameLookupResult."

	<primitive: 'primitiveResolverStartNameLookup' module: 'SocketPlugin'>
	self primitiveFailed
