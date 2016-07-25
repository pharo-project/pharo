options
numberOfRetries
	"Return how many times I will retry a failed request.
	Defaults to 0."
	
	^ self optionAt: #numberOfRetries ifAbsent: [ 0 ]