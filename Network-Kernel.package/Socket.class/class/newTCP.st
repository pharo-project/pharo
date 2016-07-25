instance creation
newTCP
	"Create a socket and initialise it for TCP"
	self initializeNetwork.
	^[ super new initialize: TCPSocketType ]
		repeatWithGCIf: [ :socket | socket isValid not ]