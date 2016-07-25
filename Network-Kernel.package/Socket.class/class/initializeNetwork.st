network initialization
initializeNetwork
	"Initialize the network drivers and the NetNameResolver. Do nothing if the network is already initialized."
	"Note: The network must be re-initialized every time Pharo starts up, so applications that persist across snapshots should be prepared to re-initialize the network as needed. Such applications should call 'Socket initializeNetwork' before every network transaction. "

	NetNameResolver initializeNetwork