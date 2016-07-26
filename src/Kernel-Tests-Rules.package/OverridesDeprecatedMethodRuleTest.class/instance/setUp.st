running
setUp

	testClass := Object newAnonymousSubclass.
	testSubclass := testClass newAnonymousSubclass.
	
	testClass compile: self methodName, ' self deprecated: ''method is deprecated'''.