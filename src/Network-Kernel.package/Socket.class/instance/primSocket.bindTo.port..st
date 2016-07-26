primitives
primSocket: socketID bindTo: anAddress port: aPort 
	"Bind socket to provided IPv4 address and port"
	<primitive: 'primitiveSocketBindToPort' module: 'SocketPlugin'>
	^self primitiveFailed