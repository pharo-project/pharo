running
tearDown

	testPackage mcPackage workingCopy unload.
	testPackage unregister.
	testPackage := nil.	
	super tearDown 