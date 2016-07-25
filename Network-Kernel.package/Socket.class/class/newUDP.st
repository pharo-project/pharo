instance creation
newUDP
	"Create a socket and initialise it for UDP"
	self initializeNetwork.
	^[ super new initialize: UDPSocketType ]
		repeatWithGCIf: [ :socket | socket isValid not ]