testing
canReadContent
	^ self hasContentLength or: [ self isChunked or: [ self allowsReadingUpToEnd ] ]