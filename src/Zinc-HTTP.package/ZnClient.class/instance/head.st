operations
head
	"Execute a HTTP HEAD on the request set up and return nil."

	^ self 
		method: #HEAD; 
		execute