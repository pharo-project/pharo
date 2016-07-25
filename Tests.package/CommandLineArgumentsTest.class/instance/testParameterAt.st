tests
testParameterAt
	self assert: (self commandLine argumentAt: 1) = 'noOpt1'.
	self assert: (self commandLine argumentAt: 4) = '--option1'.
	self deny: (self commandLine argumentAt: 5) = 'foo'.
	self deny: (self commandLine argumentAt: 1) isNil.