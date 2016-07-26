instance creation
empty
	^ self new
		requestLine: ZnRequestLine empty;
		headers: ZnHeaders defaultRequestHeaders;
		yourself