ZnClientTransactionEvent is a ZnClientLogEvent that notes the completion of a client side HTTP transaction (request/response).

Instance Variables
	request: a ZnRequest
	requestDuration:	 milliseconds
	response:	a ZnResponse
	responseDuration: milliseconds
	
Note that this can be quite large since it includes the entities transferred. See ZnSimplifiedClientTransactionEvent for a more compact representation.