operations
get
	"Execute an HTTP GET the request set up and return the response #contents."
	
	^ self 
		method: #GET; 
		execute