tests
testDemoteToRPackageNamedMultilevelPackage2
	| package1 package2  class  |

	package1 := (RPackage named: #'Test1-TAG1-X1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1-X1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToRPackageNamed: 'Test1-TAG1'.

	self deny: (RPackage organizer includesPackage: package1).
	package2 := RPackage organizer packageNamed: 'Test1-TAG1'.
	self assert: package2 notNil. 
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'X1') classes includes: class).