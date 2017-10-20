tests
testSearchString

	| anchor |
	
	anchor := ReVarSearchSourceAnchor
		entity: Object newAnonymousSubclass
		string: 'var'.
		
	self
		assert: anchor searchString
		equals: 'var'