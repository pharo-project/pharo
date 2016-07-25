values
testTimestamp
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	
	1 to: 10 do:[:i| WPDummy new exampleAssignment: i].
	1 to: 9 do: [:i|  
		|older newer|
		older := watchpoint values at: i.
		newer := watchpoint values at: i + 1.
		self assert: (older key <= newer key).
	]. 
	
	watchpoint uninstall.
	