accessing
useConnection: connection
	"Hook method that can be overwritten to give subclasses the chance to 
	keep using connection in the current thread/process after the server wrote the response"