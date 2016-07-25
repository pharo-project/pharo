tests
testAddKeymapCreatesCategory
	self assert: KMRepository default categories isEmpty.
	
	KMRepository default
		initializeKeymap: #test
		executingOn: $r ctrl, $r asKeyCombination, $r asKeyCombination
		doing: [ :receiver | "nothing" ]
		inCategory: #Testing
		platform: #all.
	
	self assert: (KMRepository default includesCategoryNamed: #Testing).
	self assert: KMRepository default categories size = 1.