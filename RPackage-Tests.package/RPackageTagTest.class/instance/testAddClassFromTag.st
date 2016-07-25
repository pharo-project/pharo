tests
testAddClassFromTag
	| package1 package2 class |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	
	self assert: (package1 includesClass: class).
	self assert: (package1 classTagNamed: #TAG1 ifAbsent: [ nil ]) notNil.
	self assert: ((package1 classTagNamed: #TAG1 ifAbsent: [ nil ]) includesClass: class).
	
	package2 := (RPackage named: #Test2) register.
	
	(package2 addClassTag: #TAG2) addClass: class.
	
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).	
	self assert: (package2 classTagNamed: #TAG2 ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #TAG2 ifAbsent: [ nil ]) includesClass: class).

	
