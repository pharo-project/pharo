tests
testPromoteAsRPackageWithExtension
	| packageOriginal packagePromoted class classOther tag |

	packageOriginal := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: #'accessing'.

	classOther := self createNewClassNamed: 'TestClassOther' inCategory: 'XXXX'.
	classOther compile: 'bar ^42' classified: #'*Test1-TAG1'.

	tag := packageOriginal classTagNamed: 'TAG1'.
	tag promoteAsRPackage.

	packagePromoted:= RPackage organizer packageNamed: 'Test1-TAG1'.	
	self assert: packagePromoted notNil. 
	self assert: (packagePromoted classes includes: class).
	self assert: (packagePromoted extensionMethods includes: classOther>>#bar).	
	self assert: (classOther>>#bar) protocol equals: '*Test1-TAG1'. 
	self deny: (packageOriginal classes includes: class).
	self deny: (packageOriginal extensionMethods includes: classOther>>#bar)
	