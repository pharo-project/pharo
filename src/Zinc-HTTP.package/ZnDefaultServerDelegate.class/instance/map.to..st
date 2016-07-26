accessing
map: prefix to: handler
	"Arrange for the receiver to dispatch requests matching prefix to handler.
	Handler can be a symbol naming a method in the receiver, 
	or a block accepting the request and producing the response.
	The special prefix '/' is resolved first and can refer to another prefix."
	
	self prefixMap 
		at: prefix
		put: handler
		