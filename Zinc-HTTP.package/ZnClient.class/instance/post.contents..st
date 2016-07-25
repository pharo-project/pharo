convenience
post: url contents: object
	"Execute an HTTP POST request on url with object as request #contents:
	and return the response #contents."

	^ self 
		url: url; 
		contents: object; 
		post