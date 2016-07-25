options
connectionReuseTimeout
	"Return the timeout in seconds currently used to determine
	if I should try to reuse an open connection. Defaults to 10 seconds."

	^ self optionAt: #connectionReuseTimeout ifAbsent: [ 10 ]