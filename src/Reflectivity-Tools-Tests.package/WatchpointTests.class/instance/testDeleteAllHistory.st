deleting history
testDeleteAllHistory
	|node1 node2 watchpoint1 watchpoint2|
	
	node1 := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint1 := Watchpoint in: node1.
	node2 := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint2 := Watchpoint in: node2.
	
	WPDummy new exampleAssignment: 2.
	WPDummy new exampleAssignment.
		
	self assert: (watchpoint1 values first value = 2).
	self assert: (watchpoint2 values first value = 1).
	
	Watchpoint deleteAllHistory.
	
	self assert: watchpoint1 values isEmpty.
	self assert: watchpoint2 values isEmpty.
	
	watchpoint1 uninstall.
	watchpoint2 uninstall.
	