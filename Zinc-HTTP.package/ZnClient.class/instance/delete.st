operations
delete
	"Execute a HTTP DELETE ont the request set up and return the response #contents."
	
	^ self 
		method: #DELETE; 
		execute