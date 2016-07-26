private protocol
checkResponse: aResponse onError: errorBlock onWarning: warningBlock
	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."

	self responseIsError
		ifTrue: [errorBlock value: aResponse].
	self responseIsWarning
		ifTrue: [warningBlock value: aResponse].
