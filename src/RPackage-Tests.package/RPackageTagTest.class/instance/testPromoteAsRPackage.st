tests
testPromoteAsRPackage
	| package1 package2 class tag1 |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	tag1 := package1 classTagNamed: 'TAG1'.
	
	tag1 promoteAsRPackage.

	package2:= RPackage organizer packageNamed: 'Test1-TAG1'.	
	self assert: package2 notNil. 
	self assert: (package2 classes includes: class).
	self deny: (package1 classes includes: class).