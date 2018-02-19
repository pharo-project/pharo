"
SUnit tests for RPackage tags
"
Class {
	#name : #RPackageTagTest,
	#superclass : #RPackageTestCase,
	#category : #'RPackage-Tests'
}

{ #category : #running }
RPackageTagTest >> tearDown [ 
	
	#(TestClass TestClassOther) 
		do: [ :each | 
			self class environment at: each ifPresent: #removeFromSystem ].
	super tearDown.	
]

{ #category : #tests }
RPackageTagTest >> testAddClass [
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

	

]

{ #category : #tests }
RPackageTagTest >> testAddClassFromTag [
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

	

]

{ #category : #tests }
RPackageTagTest >> testAsRPackage [
	| package1 tag convertedTag class |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.
	
	tag := (package1 classTagNamed: #TAG1).
	
	convertedTag := tag asRPackage.
	
	self assert: (convertedTag includesClass: class).
	self assert: (convertedTag includesSelector: 'foo' ofClass:  class).
]

{ #category : #tests }
RPackageTagTest >> testAsRPackageWithExtensionMethods [
	| package1 convertedTag class  |

	package1 := (RPackage named: #Test1) register.
	package1 addClassTag: #TAG1.
	
	(RPackage named: #Test2) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test2'.
	
	class compile: 'foo ^42' classified: '*Test1-TAG1'.
	
	convertedTag := (package1 classTagNamed: #TAG1) asRPackage.
	
	self assert: (convertedTag definedClasses isEmpty).
	self assert: (convertedTag includesExtensionSelector: 'foo' ofClass:  class).
]

{ #category : #tests }
RPackageTagTest >> testPromoteAsRPackage [
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
]

{ #category : #tests }
RPackageTagTest >> testPromoteAsRPackageWithExtension [
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
	
]
