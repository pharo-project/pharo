tests
testErrorWithErrorHandler
	self runSimulated: [[self methodWithError] on: Error do: [:err | ]]  