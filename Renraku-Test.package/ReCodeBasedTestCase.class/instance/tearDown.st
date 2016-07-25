running
tearDown

	testPackage methods do: #removeFromSystem.
	testPackage definedClasses do: #removeFromSystem.
	super tearDown