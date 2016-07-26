tests
testSetMaximumSizeUnbounded
		
	| u |
	u := fullCache instVarNamed: #used.
	fullCache maximumSize: nil. "unbounded"
	self assert: u = (fullCache instVarNamed: #used).
	self validateSizes: cache.
	self validateCollections: cache
