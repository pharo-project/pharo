running
tearDown
	super tearDown.
	
	package1 unregister.
	package2 unregister.
	MCPackageManager registry
		removeKey: mcPackage1 ifAbsent: [ ];
		removeKey: mcPackage2 ifAbsent: [ ] 