convenience
delete: url
	"Execute an HTTP DELETE request on url and return the response #contents."
	
	^ self 
		url: url; 
		delete