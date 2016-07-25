values
testSaveOneValue
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment.
	
	self assert: (watchpoint values first value = 1).
	
	watchpoint uninstall.
	