adding
testWatchpointTwice
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	
	self should: [watchpoint install] raise: Error.
	
	watchpoint uninstall.