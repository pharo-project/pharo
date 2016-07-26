tests
testFromHeadDetectIfNotFound

	" Assert for getting 42 when entry is not found. "
	self
		assert: (log
			fromHeadDetect: [ :dummy | false ]
			ifNotFound: [ 42 ])
		equals: 42.