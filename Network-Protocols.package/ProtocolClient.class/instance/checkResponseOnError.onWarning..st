private protocol
checkResponseOnError: errorBlock onWarning: warningBlock
	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."

	self fetchPendingResponse.
	self checkResponse: self lastResponse onError: errorBlock onWarning: warningBlock