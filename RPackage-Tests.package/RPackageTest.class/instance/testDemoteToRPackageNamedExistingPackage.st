tests
testDemoteToRPackageNamedExistingPackage
	| package1 package2 packageExisting class  |

	package1 := (RPackage named: #'Test1-TAG1') register.
	packageExisting := (RPackage named: #'Test1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToRPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: package1).
	package2 := RPackage organizer packageNamed: 'Test1'.
	self assert: package2 notNil. 
	self assert: package2 equals: packageExisting. 
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class).