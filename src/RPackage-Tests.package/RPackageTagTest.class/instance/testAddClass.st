tests
testAddClass
	| package1 package2 class |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1'.
	
	self assert: (package1 includesClass: class).
	
	package2 := (RPackage named: #Test2) register.
	
	(package2 addClassTag: #TAG) addClass: class.
	
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).	
	self assert: (package2 classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).

	
