running
tearDown

	self tearDownscreamerRule.
	testPackage unregister.
	testPackage := nil.	
	super tearDown 