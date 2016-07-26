tests
testVersion23

	self assert: (MetacelloVersionNumber fromString: '3.0.0.-rc.1') = (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0') > (MetacelloVersionNumber fromString: '3.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3') > (MetacelloVersionNumber fromString: '3-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.-rc.1') = (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0.-rc.1') = (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3') > (MetacelloVersionNumber fromString: '3.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0') > (MetacelloVersionNumber fromString: '3.0.0-rc.1').

