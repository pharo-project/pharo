tests
testFromHeadDetect

	" Assert for head "
	self
		assert: (log fromHeadDetect: [ :dummy | true ])
		equals: log head.