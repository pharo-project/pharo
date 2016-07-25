values
testSendMessage
	|node watchpoint|
	
	node := (WPDummy>>#exampleMessageSend) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleMessageSend.
	
	self assert: (watchpoint values first value = 5).
	
	watchpoint uninstall.
	