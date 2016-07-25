options
enforceAcceptContentType
	"Return whether or not I enforce that the content type of the
	response matches the content type specified in the accept request header."
	
	^ self optionAt: #enforceAcceptContentType ifAbsent: [ false ]