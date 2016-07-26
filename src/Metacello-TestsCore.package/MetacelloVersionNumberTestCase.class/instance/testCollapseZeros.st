tests
testCollapseZeros

	self assert: (MetacelloVersionNumber fromString: '1.0-beta.24.0.1') collapseZeros printString = '1-beta.24.0.1'.
	self assert: (MetacelloVersionNumber fromString: '1.0-beta.24.0.0.1') collapseZeros printString = '1-beta.24.0.0.1'.
	self assert: (MetacelloVersionNumber fromString: '1.0.0-beta.24.0.0.1') collapseZeros printString = '1-beta.24.0.0.1'.
