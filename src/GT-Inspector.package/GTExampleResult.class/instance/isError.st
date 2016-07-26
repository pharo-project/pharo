testing
isError
	^ self isSuccess not
		and: [ self unexpectedError isExampleError ]