tests
testAsRPackageWithExtensionMethods
	| package1 convertedTag class  |

	package1 := (RPackage named: #Test1) register.
	package1 addClassTag: #TAG1.
	
	(RPackage named: #Test2) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test2'.
	
	class compile: 'foo ^42' classified: '*Test1-TAG1'.
	
	convertedTag := (package1 classTagNamed: #TAG1) asRPackage.
	
	self assert: (convertedTag definedClasses isEmpty).
	self assert: (convertedTag includesExtensionSelector: 'foo' ofClass:  class).