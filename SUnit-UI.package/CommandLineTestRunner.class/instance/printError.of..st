helper
printError: anError of: aTestCase 
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [ERROR]' 
		of: aTestCase 