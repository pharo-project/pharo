operations
options
	"Execute an HTTP OPTIONS on the request set up and return the response #contents."
	
	^ self 
		method: #OPTIONS; 
		execute