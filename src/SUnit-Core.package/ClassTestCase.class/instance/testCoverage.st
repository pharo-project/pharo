tests
testCoverage

	| untested | 
	self class mustTestCoverage ifTrue:
		[ untested := self selectorsNotTested.
		self assert: untested isEmpty 
		description: untested size asString, ' selectors are not covered' ]