operations
performOperation: operation with: argument on: url
	"Search for a subclass that can handle operation with argument on url.
	Instanciate the operation, execute it and return the result.
	Signal an Error when no matching handler is found."
	
	| handlerClass |
	handlerClass := self handlerForOperation: operation with: argument on: url.
	^ handlerClass new
		url: url;
		argument: argument;
		performOperation 