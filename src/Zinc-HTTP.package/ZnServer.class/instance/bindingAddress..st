options
bindingAddress: address
	"Set the interface address we will be listening on.
	Specify nil to listen on all/any interfaces, the default.
	Address must be a 4 element ByteArray, like #[127 0 0 1].
	Cannot be changed after the server is already running."
	
	self optionAt: #bindAddress put: address