tests
testDemoteToRPackageNamedWithExtension
	| packageOriginal packageDemoted class classOther |

	packageOriginal := (RPackage named: #'Test1-TAG1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	classOther := self createNewClassNamed: 'TestClassOther' inCategory: 'XXXX'.
	classOther compile: 'bar ^42' classified: #'*Test1-TAG1'.

	packageOriginal demoteToRPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: packageOriginal).
	packageDemoted := RPackage organizer packageNamed: 'Test1'.
	self assert: packageDemoted notNil. 
	self assert: (packageDemoted classes includes: class).
	self assert: ((packageDemoted classTagNamed: 'TAG1') classes includes: class).
	self assert: (packageDemoted extensionMethods includes: classOther>>#bar).	
	self assert: ((classOther>>#bar) protocol) equals: '*Test1-TAG1'.
	self assert: (packageDemoted classes	includesAll: {class. classOther}).
