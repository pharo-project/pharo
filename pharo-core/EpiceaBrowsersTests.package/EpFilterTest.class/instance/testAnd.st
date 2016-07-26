tests
testAnd
	| aFilter |
	aFilter := EpAndFilter withAll: { 
		EpPluggableFilter codeChangeFilter. 
		EpPluggableFilter methodFilter }.
	
	self 
		assert: aFilter
		accepts: self methodAddition
		rejects: self classAddition