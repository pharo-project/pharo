utilities
equalityTestFor: aChange
	self assert: aChange = aChange.
	self assert: aChange hash = aChange hash.

	self assert: aChange copy = aChange.
	self assert: aChange copy hash = aChange hash