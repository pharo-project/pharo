options
connectionReuseTimeout: seconds
	"Set the timeout used to determine if I should try 
	to reuse an open connection to seconds."

	self optionAt: #connectionReuseTimeout put: seconds