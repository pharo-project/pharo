accessing
dependencies
	dependencies ifNil: [self loadDependencies].
	^ dependencies