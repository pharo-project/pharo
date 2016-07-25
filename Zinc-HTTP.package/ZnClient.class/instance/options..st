convenience
options: url
	"Execute an HTTP OPTIONS request on url and return the response #contents."

	^ self 
		url: url; 
		options