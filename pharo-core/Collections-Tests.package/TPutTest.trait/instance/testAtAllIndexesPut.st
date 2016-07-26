tests - puting with indexes
testAtAllIndexesPut
	
	self nonEmpty atAllPut: self aValue.
	self nonEmpty do:[ :each| self assert: each = self aValue].
	