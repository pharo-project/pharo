operations
execute
	"Execute the currently set up request to generate a response.
	Return the #contents of the response, if any."

	| result |
	self prepareRequest.
	self withProgressDo: [ result := self executeWithTimeout ].
	self prepareNextRequest.
	^ result