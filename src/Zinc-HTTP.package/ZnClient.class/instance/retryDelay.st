options
retryDelay
	"Return the number of seconds I will delay retrying a failed request,
	provided I do retry requests. Defaults to 1 seconds."
	
	^ self optionAt: #retryDelay ifAbsent: [ 1 ]