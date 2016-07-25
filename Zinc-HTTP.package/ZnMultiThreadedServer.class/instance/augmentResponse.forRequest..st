request handling
augmentResponse: response forRequest: request
	"Our handler has produced response for request, manipulate the response before writing it"

	super augmentResponse: response forRequest: request.
	response setKeepAliveFor: request.
	response setConnectionCloseFor: request