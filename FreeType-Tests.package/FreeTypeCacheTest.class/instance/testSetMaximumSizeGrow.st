tests
testSetMaximumSizeGrow
	| u m |
	
	u := fullCache instVarNamed: #used.
	m := fullCache instVarNamed: #maximumSize.
	fullCache maximumSize: m * 2 . "grow"
	self assert: u = (fullCache instVarNamed: #used).	
	self validateSizes: cache.
	self validateCollections: cache