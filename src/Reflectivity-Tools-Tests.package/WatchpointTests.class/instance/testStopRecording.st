recording
testStopRecording
	|node watchpoint dummy|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	dummy := WPDummy new.
	
	dummy exampleAssignment: 1.
	dummy exampleAssignment: 2.
	self assert: (watchpoint values size = 2).
	self assert: (watchpoint values second value = 2).
	
	watchpoint stop.
	dummy exampleAssignment: 3.
	self assert: (watchpoint values size = 2).
	
	watchpoint start.
	dummy exampleAssignment: 4.
	self assert: (watchpoint values size = 3).
	self assert: (watchpoint values third value = 4).
	
	watchpoint uninstall.
	
	self deny: (node hasWatchpoint)
	