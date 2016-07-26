helper
printFailure: anError of: aTestCase 
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [FAIL]' 
		of: aTestCase 