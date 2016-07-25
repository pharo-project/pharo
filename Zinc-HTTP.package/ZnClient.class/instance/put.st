operations
put
	"Execute a HTTP PUT on the request set up and return the response #contents."

	^ self 
		method: #PUT; 
		execute