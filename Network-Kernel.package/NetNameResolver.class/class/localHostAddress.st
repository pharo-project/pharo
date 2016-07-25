lookups
localHostAddress
	"Return the local address of this host."
	"NetNameResolver localHostAddress"

	self initializeNetwork.
	^ self primLocalAddress
