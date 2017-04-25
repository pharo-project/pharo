testing
hasContentLength
	^ self headers hasContentLength and: [ self headers contentLength > 0 ]