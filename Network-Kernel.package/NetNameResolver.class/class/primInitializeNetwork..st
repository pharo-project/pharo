network initialization
primInitializeNetwork: resolverSemaIndex
	"Initialize the network drivers on platforms that need it, such as the Macintosh, and return nil if network initialization failed or the reciever if it succeeds. Since mobile computers may not always be connected to a network, this method should NOT be called automatically at startup time; rather, it should be called when first starting a networking application. It is a noop if the network driver has already been initialized. If non-zero, resolverSemaIndex is the index of a VM semaphore to be associated with the network name resolver. This semaphore will be signalled when the resolver status changes, such as when a name lookup query is completed."
	"Note: some platforms (e.g., Mac) only allow only one name lookup query at a time, so a manager process should be used to serialize resolver lookup requests."

	<primitive: 'primitiveInitializeNetwork' module: 'SocketPlugin'>
	^ nil  "return nil if primitive fails"
