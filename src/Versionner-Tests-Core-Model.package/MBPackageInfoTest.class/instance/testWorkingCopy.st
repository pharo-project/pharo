tests
testWorkingCopy

	|  workingCopy |
	self assert: (RPackageOrganizer default packageNamed: 'FooBarZork' ifAbsent: []) isNil.	
	package := MBPackageInfo named: 'FooBarZork'.
	workingCopy := package workingCopy.
	"creating a packageInfo for a package that is not currently loaded can create problems. see Metacello Issue 113"
	self assert: workingCopy isNil.
