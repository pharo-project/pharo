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
	packageWithoutClassTags := Smalltalk organization packageOf: StartupAction.
	self denyEmpty: packageWithoutClassTags classTags.
	self assertEmpty: packageWithoutClassTags actualClassTags.

	packageWithClassTags := Smalltalk organization packageOf: Object.
	self assert: packageWithClassTags actualClassTags equals: packageWithClassTags classTags
]

{ #category : #tests }
RPackageTest >> testAddClass [
	| package1 package2 class done |

	package1 := (RPackage named: #Test1) register.
	done := 0.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.
	SystemAnnouncer uniqueInstance when: ClassRecategorized do: [ done := done + 1] for: self.

	self assert: (package1 includesClass: class).
	self assert: (package1 classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package1 classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).

	package2 := (RPackage named: #Test2) register.

	package2 addClass: class.

	self assert: done equals: 1.
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 classTagNamed: #Test2 ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #Test2 ifAbsent: [ nil ]) includesClass: class)
]

{ #category : #tests }
RPackageTest >> testAddClassFromTag [
	| package1 package2 class |

	package1 := (self packageClass named: #Test1) register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.

	self assert: (package1 includesClass: class).

	package2 := (self packageClass named: #Test2) register.

	package2 addClass: class.

	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 classTagNamed: #Test2 ifAbsent: [ nil ]) notNil.
	self assert: ((package2 classTagNamed: #Test2 ifAbsent: [ nil ]) includesClass: class)
]

{ #category : #tests }
RPackageTest >> testAllUnsentMessages [
	| package class1 class2 |

	package := (RPackage named: #Test1) register.

	class1 := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.
	class2 := self createNewClassNamed: 'TestClassOther' inCategory: 'Test1-TAG'.

	class1
		compile: 'nonexistingMethodName1 42';
		compile: 'nonexistingMethodName2 42'.

	class2
		compile: 'nonexistingMethodName1 42';
		compile: 'nonexistingMethodName3 42';
		compile: 'nonexistingMethodName4 class1 new nonexistingMethodName2'.

	self assert: package allUnsentMessages size equals: 3.

	self assert: package allUnsentMessages equals: (#('nonexistingMethodName1' 'nonexistingMethodName3' 'nonexistingMethodName4') collect: [:each | each asSymbol]) asSet
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

	package1 := (self packageClass named: #'Test1-TAG1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackageNamed: 'Test1'.

	self deny: (self packageClass organizer includesPackage: package1).
	package2 := self packageClass organizer packageNamed: 'Test1'.
	self assert: package2 notNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class)
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedExistingPackage [
	| package1 package2 packageExisting class  |

	package1 := (self packageClass named: #'Test1-TAG1') register.
	packageExisting := (self packageClass named: #'Test1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackageNamed: 'Test1'.

	self deny: (self packageClass organizer includesPackage: package1).
	package2 := self packageClass organizer packageNamed: 'Test1'.
	self assert: package2 notNil.
	self assert: package2 equals: packageExisting.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class)
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedMultilevelPackage1 [
	| package1 package2  class  |

	package1 := (self packageClass named: #'Test1-TAG1-X1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1-X1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackageNamed: 'Test1'.

	self deny: (self packageClass organizer includesPackage: package1).
	package2 := self packageClass organizer packageNamed: 'Test1'.
	self assert: package2 notNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1-X1') classes includes: class)
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedMultilevelPackage2 [
	| package1 package2  class  |

	package1 := (self packageClass named: #'Test1-TAG1-X1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1-X1'.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackageNamed: 'Test1-TAG1'.

	self deny: (self packageClass organizer includesPackage: package1).
	package2 := self packageClass organizer packageNamed: 'Test1-TAG1'.
	self assert: package2 notNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'X1') classes includes: class)
]

{ #category : #tests }
RPackageTest >> testDemoteToRPackageNamedWithExtension [
	| packageOriginal packageDemoted class classOther |

	packageOriginal := (RPackage named: #'Test1-TAG1') register.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	classOther := self createNewClassNamed: 'TestClassOther' inCategory: 'XXXX'.
	classOther compile: 'bar ^42' classified: #'*Test1-TAG1'.

	packageOriginal demoteToTagInPackageNamed: 'Test1'.

	self deny: (RPackage organizer includesPackage: packageOriginal).
	packageDemoted := RPackage organizer packageNamed: 'Test1'.
	self assert: packageDemoted notNil.
	self assert: (packageDemoted classes includes: class).
	self assert: ((packageDemoted classTagNamed: 'TAG1') classes includes: class).
	self assert: (packageDemoted extensionMethods includes: classOther>>#bar).
	self assert: ((classOther>>#bar) protocol) equals: '*Test1-TAG1'.
	self assert: (packageDemoted classes	includesAll: {class. classOther})
]

{ #category : #'tests - properties' }
RPackageTest >> testHasProperty [
	| package testValue |

	testValue := Date today.
	package := self class package.

	package propertyAt: #testKeySelector put: testValue.
	self	assert: (package hasProperty: #testKeySelector).

	package removeProperty: #testKeySelector.
	self deny: (package hasProperty: #testKeySelector)
]

{ #category : #'tests - queries' }
RPackageTest >> testHierarchyRoots [

	| roots |
	roots := (Smalltalk organization packageNamed: #'RPackage-Tests') hierarchyRoots.
	roots := roots collect: [ :each | each name ].
	#( RPackageTestCase ) do: [ :each | roots includes: each ]
]

{ #category : #tests }
RPackageTest >> testIsTestPackage [

	|packages |
	packages := self createMockTestPackages.
	"Happy case: test package 'MockPackage-Tests' must contain -Tests suffix."
	self assert: packages first isTestPackage equals: true.

	"Package 'MockPackage-tests' is not test package, since it has lowercase suffix."
	self assert: packages second isTestPackage  equals: false.

	"Happy case: regular package 'MockPackage' without -Tests suffix is not a test package."
	self assert: packages third isTestPackage  equals: false.

	"Package 'MockPackage-Tests-Package' containting -Tests- in middle, so it is test package."
	self assert: packages fourth isTestPackage equals: true.

	"cleanup of inst.vars should be done in tearDown"
]

{ #category : #'tests - MC' }
RPackageTest >> testMcPackage [

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcPackage equals: (MCPackage new name: #Test1)
]

{ #category : #'tests - MC' }
RPackageTest >> testMcWorkingCopy [

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcWorkingCopy equals: (MCWorkingCopy forPackage: (MCPackage new name: #Test1))
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
	self assert: package properties isNil
]

{ #category : #tests }
RPackageTest >> testRenameToMakesMCDirty [
	| package |

	package := (self packageClass named: #'Test1') register.
	self createNewClassNamed: 'TestClass' inCategory: 'Test1'.

	package renameTo: 'Test2'
]

{ #category : #'tests - queries' }
RPackageTest >> testRoots [

	| roots |
	roots := (Smalltalk organization packageNamed: #'RPackage-Tests') roots.
	roots := roots collect: [ :each | each name ].
	#( RPackageStringExtensionTest RPackageRenameTest TestRPackagePrequisites RPackageTestCase RPackageWithDoTest ) do: [ :each | roots includes: each ]
]
