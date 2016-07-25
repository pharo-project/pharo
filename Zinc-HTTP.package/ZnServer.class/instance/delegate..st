options
delegate: object
	"Set the delegate to object. Can be nil.
	This will be sent #handleRequest: to handle a request and produce a response"
	
	self optionAt: #delegate put: object