tests
testAddKeymapCreatesShortcut
	KMRepository default
		initializeKeymap: #test
		executingOn: $r ctrl, $r asKeyCombination, $r asKeyCombination
		doing: [ :receiver | "nothing" ]
		inCategory: #Testing
		platform: #all.
	
	self assert: (KMRepository default categoryForName: #Testing ) allEntries size = 1.