deleting history
testDeleteHistoryFromNode
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment: 1.
	WPDummy new exampleAssignment: 2.
	
	self assert: (watchpoint values first value = 1).
	self assert: (watchpoint values second value = 2).
	
	Watchpoint deleteHistoryFrom: node.
	self assert: watchpoint values isEmpty.
	
	watchpoint uninstall.
	