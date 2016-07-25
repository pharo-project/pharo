ZnServerTransactionEvent is a ZnServerLogEvent that notes the completion of a server side HTTP transaction (request/response).

Instance Variables
	request: a ZnRequest
	response:	a ZnResponse
	timing: a ZnServerTransactionTiming

Note that this can be quite large since it includes the entities transferred. See ZnSimplifiedServerTransactionEvent for a more compact representation.