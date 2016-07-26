options
delegate
	"Return the optional delegate, 
	the object that will be sent #handleRequest: to handle a request and produce a response.
	The default delegate is ZnDefaultServerDelegate"
	
	^ self optionAt: #delegate ifAbsentPut: [ ZnDefaultServerDelegate new ]