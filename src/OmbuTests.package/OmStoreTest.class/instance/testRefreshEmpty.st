tests
testRefreshEmpty
	store refresh.
	
	self assert: store entries isEmpty.