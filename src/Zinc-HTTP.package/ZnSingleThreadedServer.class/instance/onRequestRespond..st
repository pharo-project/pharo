accessing
onRequestRespond: block
	"Convenience method to quickly set up a delegate.
	On any incoming request, block will be executed with the request 
	as argument to produce a valid response."
	
	self delegate: (ZnValueDelegate with: block)  