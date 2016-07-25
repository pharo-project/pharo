tests
testFromDetectIfNotFound
	| reference |
	reference := log priorReferenceAt: log head.
	" Assert for getting 42 when entry is not found. "
	self
		assert: (log
			from: reference
			detect: [ :dummy | false ]
			ifNotFound: [ 42 ])
		equals: 42.