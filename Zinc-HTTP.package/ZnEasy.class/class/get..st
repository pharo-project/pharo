operations
get: urlObject
	"self get: 'http://caretaker.wolf359.be/small.html'."
	
	^ self client
		url: urlObject;
		get;
		response
	