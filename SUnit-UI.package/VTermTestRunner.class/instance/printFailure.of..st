helper
printFailure: anError of: aTestCase 
	self stderr yellow.
	^ super printFailure: anError of: aTestCase 