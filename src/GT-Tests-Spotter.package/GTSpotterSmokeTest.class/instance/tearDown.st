running
tearDown
	super tearDown.
	
	window ifNotNil: [ window delete ]