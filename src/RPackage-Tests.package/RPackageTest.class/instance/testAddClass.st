tests
testAddClass
	| package1 package2 class done |

	package1 := (RPackage named: #Test1) register.
	done := 0.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.
	SystemAnnouncer uniqueInstance when: ClassRecategorized do: [ done := done + 1].
	
	self assert: (package1 includesClass: class).
	self assert: (package1 classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package1 classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	
	package2 := (RPackage named: #Test2) register.
	
	package2 addClass: class.
	
	self assert: done = 1.
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).	
	self assert: (package2 classTagNamed: #Test2 ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #Test2 ifAbsent: [ nil ]) includesClass: class).