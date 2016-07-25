convenience
get: url
	"Execute an HTTP GET request on url and return the response #contents."

	^ self 
		url: url; 
		get