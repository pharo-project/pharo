convenience
head: url
	"Execute an HTTP HEAD request on url and return nil."

	^ self 
		url: url; 
		head