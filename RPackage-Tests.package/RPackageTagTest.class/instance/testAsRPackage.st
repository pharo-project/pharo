tests
testAsRPackage
	| package1 tag convertedTag class |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.
	
	tag := (package1 classTagNamed: #TAG1).
	
	convertedTag := tag asRPackage.
	
	self assert: (convertedTag includesClass: class).
	self assert: (convertedTag includesSelector: 'foo' ofClass:  class).