adding
testAddSameWatchpointSameNode
	|node watchpoint watchpoint2|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	watchpoint2 := Watchpoint in: node.
	
	self assert: (watchpoint = watchpoint2).
	
	watchpoint uninstall.
	
	self deny: (node hasWatchpoint)
	