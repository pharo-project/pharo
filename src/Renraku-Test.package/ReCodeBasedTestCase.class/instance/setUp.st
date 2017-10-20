running
setUp
	super setUp.
	testClass := Object
		subclass: 'NuclearSiteTestingClass'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: testPackage name.
	testMethod := testClass >> (testClass compile: 'nuclearSiteTestingMethod ^ self').
	
	testClass2 := Object
		subclass: 'NuclearSiteTestingClass2'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: testPackage name.