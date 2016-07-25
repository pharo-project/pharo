accessing
environment: namespace

	namespace = self class environment
		ifFalse:[ self annotationNamed: #environment put: namespace ]