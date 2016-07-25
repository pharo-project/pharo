public
value: request
	"I implement the generic #value: message as equivalent to #handleRequest:"
	
	^ self handleRequest: request