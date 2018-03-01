"
SUnit tests for RPackage 
"
Class {
	#name : #RPackageTest,
	#superclass : #RPackageTestCase,
	#category : #'RPackage-Tests'
}

{ #category : #running }
RPackageTest >> tearDown [ 
	
	#(TestClass TestClassOther) 
		do: [ :each | 
			self class environment at: each ifPresent: #removeFromSystem ].
	super tearDown	
]

{ #category : #tests }
RPackageTest >> testActualClassTags [

	| packageWithoutClassTags packageWithClassTags |
	packageWithoutClassTags := RPackageOrganizer default packageOf: Announcement.
	self deny: packageWithoutClassTags classTags isEmpty.
	self assert: packageWithoutClassTags actualClassTags isEmpty.
	
	packageWithClassTags := RPackageOrganizer default packageOf: Object.
	self assert: packageWithClassTags actualClassTags equals: packageWithClassTags classTags
]

{ #category : #tests }
RPackageTest >> testAddClass [
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
]

{ #category : #tests }
RPackageTest >> testAddClassFromTag [
	| package1 package2 class |

	package1 := (RPackage named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.
	
	self assert: (package1 includesClass: class).
	
	package2 := (RPackage named: #Test2) register.
	
	package2 addClass: class.
	
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).	
	self assert: (package2 classTagNamed: #Test2 ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #Test2 ifAbsent: [ nil ]) includesClass: class).

	

]

{ #category : #tests }
RPackageTest >> testAnonymousClassAndSelector [
	"jgeidel test case from issue 12628."

	"Make sure we don't have a registration or a package for the method."

	| ghost method uPackage |
	ghost := Object newAnonymousSubclass.
	uPackage := self organizer packageNamed: #'_UnpackagedPackage'.
	method := ghost compiler compile: 'rpackagetest'.
	ghost addSelector: #rpackagetest withMethod: method.
	self deny: (uPackage includesDefinedSelector: #rpackagetest ofClass: ghost).
	self deny: (self organizer packageOfClassNamed: ghost name) notNil.
	ghost organization classify: #rpackagetest under: '*rpackagetest'
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamed [
	| package1 package2 class  |

	package1 := (RPackage named: #'Test1-TAG1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToRPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: package1).
	package2 := RPackage organizer packageNamed: 'Test1'.
	self assert: package2 notNil. 
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class).
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedExistingPackage [
	| package1 package2 packageExisting class  |

	package1 := (RPackage named: #'Test1-TAG1') register.
	packageExisting := (RPackage named: #'Test1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToRPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: package1).
	package2 := RPackage organizer packageNamed: 'Test1'.
	self assert: package2 notNil. 
	self assert: package2 equals: packageExisting. 
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class).
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedMultilevelPackage1 [
	| package1 package2  class  |

	package1 := (RPackage named: #'Test1-TAG1-X1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1-X1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToRPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: package1).
	package2 := RPackage organizer packageNamed: 'Test1'.
	self assert: package2 notNil. 
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1-X1') classes includes: class).
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedMultilevelPackage2 [
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
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedWithExtension [
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

]

{ #category : #'tests-MC' }
RPackageTest >> testMcPackage [

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcPackage equals: (MCPackage new name: #Test1).
]

{ #category : #'tests-MC' }
RPackageTest >> testMcWorkingCopy [

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcWorkingCopy equals: (MCWorkingCopy forPackage: (MCPackage new name: #Test1)).
]

{ #category : #'tests - properties' }
RPackageTest >> testPropertyAtPut [

	| testValue package |
	
	testValue := Date today.
	package := self class package.
	
	package propertyAt: #testKeySelector put: testValue.
	self
		assert: (package propertyAt: #testKeySelector)
		equals: testValue.

	package removeProperty: #testKeySelector.
	self assert: package properties isNil.
]

{ #category : #tests }
RPackageTest >> testRenameToMakesMCDirty [
	| package |
	
	package := (RPackage named: #'Test1') register.
	self createNewClassNamed: 'TestClass' inCategory: 'Test1'.
	
	package renameTo: 'Test2'.
	
	
]
