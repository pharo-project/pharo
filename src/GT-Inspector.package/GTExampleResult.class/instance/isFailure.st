testing
isFailure
	^ self isSuccess not
		and: [ self unexpectedError isExampleError not ]