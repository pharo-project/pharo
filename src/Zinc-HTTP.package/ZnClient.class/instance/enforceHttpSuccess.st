options
enforceHttpSuccess
	"Return whether or not I enforce that the response 
	to the request is an HTTP success code."
	
	^ self optionAt: #enforceHttpSuccess ifAbsent: [ false ]