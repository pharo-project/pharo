adding
testAddWatchpoint
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	
	self assert: node hasWatchpoint.
	self assert: (Watchpoint allWatchpoints at: node) = watchpoint.
	
	watchpoint uninstall.
	self deny: node hasWatchpoint.
	self deny: (Watchpoint allWatchpoints includesKey: node). 
	