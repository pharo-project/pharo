class initialization
initialize
	"Socket initialize"

	"Socket Types"
	TCPSocketType := 0.
	UDPSocketType := 1.

	"Socket Status Values"
	InvalidSocket := -1.
	Unconnected := 0.
	WaitingForConnection := 1.
	Connected := 2.
	OtherEndClosed := 3.
	ThisEndClosed := 4.

	RegistryThreshold := 100. "# of sockets"