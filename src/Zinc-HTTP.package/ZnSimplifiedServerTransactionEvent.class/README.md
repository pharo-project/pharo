ZnSimplifiedServerTransactionEvent is a ZnServerLogEvent that notes the completion of a served side HTTP transaction (request/response). Only a limited number of simpler data is held. ZnServerTransactionEvent contains much more detailed data.

Instance Variables
	method: HTTP verb
	url: ZnUrl requested
	response:	numeric response code
	size: number of bytes in response
	duration: milliseconds

ZnSimplifiedServerTransactionEvent is ideal for classic HTTP logging with little overhead through its default string representation.
