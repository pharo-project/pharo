tests
testFromStringWithComments
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self 
		assert: (STON fromStringWithComments: '// comment\[1, /* comment */ 0, -1, true, false, nil] // comment' withCRs)
		equals: object