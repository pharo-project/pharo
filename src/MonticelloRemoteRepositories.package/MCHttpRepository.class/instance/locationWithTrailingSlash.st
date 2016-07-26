actions
locationWithTrailingSlash
	^ (self location endsWith: '/')
		ifTrue: [self location]
		ifFalse: [self location, '/']