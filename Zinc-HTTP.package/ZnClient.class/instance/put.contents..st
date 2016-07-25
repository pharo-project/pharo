convenience
put: url contents: object
	"Execute an HTTP PUT request on url with object as request #contents:
	and return the response #contents."

	^ self 
		url: url; 
		contents: object; 
		put