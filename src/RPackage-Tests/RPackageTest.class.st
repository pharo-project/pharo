"
SUnit tests for RPackage 
"
Class {
	#name : 'RPackageTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'running' }
RPackageTest >> tearDown [

	#(TestClass TestClassOther)
		do: [ :each |
			self class environment at: each ifPresent: #removeFromSystem ].
	super tearDown
]

{ #category : 'tests' }
RPackageTest >> testActualClassTags [

	| packageWithoutClassTags packageWithClassTags |
	packageWithoutClassTags := self packageOrganizer packageOf: StartupAction.
	self denyEmpty: packageWithoutClassTags classTags.
	self assertEmpty: packageWithoutClassTags actualClassTags.

	packageWithClassTags := self packageOrganizer packageOf: Object.
	self assert: packageWithClassTags actualClassTags equals: packageWithClassTags classTags
]

{ #category : 'tests' }
RPackageTest >> testAddClass [

	| package1 package2 class done |
	package1 := self createNewPackageNamed: #Test1.
	done := 0.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.


	self assert: (package1 includesClass: class).
	self assert: (package1 hasTag: #TAG).
	self assert: ((package1 classTagNamed: #TAG) includesClass: class).

	package2 := self createNewPackageNamed: #Test2.
	[
	SystemAnnouncer uniqueInstance when: ClassRecategorized do: [ done := done + 1 ] for: self.
	package2 addClass: class ] ensure: [ SystemAnnouncer uniqueInstance unsubscribe: self ].

	self assert: done equals: 1.
	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 hasTag: #Test2).
	self assert: ((package2 classTagNamed: #Test2) includesClass: class)
]

{ #category : 'tests' }
RPackageTest >> testAddClassFromTag [

	| package1 package2 class |
	package1 := self createNewPackageNamed: #Test1.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG'.

	self assert: (package1 includesClass: class).

	package2 := self createNewPackageNamed: #Test2.

	package2 addClass: class.

	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 hasTag: #Test2).
	self assert: ((package2 classTagNamed: #Test2) includesClass: class)
]

{ #category : 'tests' }
RPackageTest >> testAllUnsentMessages [

	| package class1 class2 |
	package := self createNewPackageNamed: #Test1.

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

	self
		assert: package allUnsentMessages
		equals: (#( 'nonexistingMethodName1' 'nonexistingMethodName3' 'nonexistingMethodName4' ) collect: [ :each | each asSymbol ]) asSet
]

{ #category : 'tests' }
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
	ghost classify: #rpackagetest under: '*rpackagetest'
]

{ #category : 'tests - demotion' }
RPackageTest >> testDemoteToRPackageNamed [

	| package1 package2 class |
	package1 := self createNewPackageNamed: #'Test1-TAG1'.
	class := self createNewClassNamed: 'TestClass' inPackage: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1'.
	self assert: package2 isNotNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class)
]

{ #category : 'tests - demotion' }
RPackageTest >> testDemoteToRPackageNamedExistingPackage [

	| package1 package2 packageExisting class |
	package1 := self createNewPackageNamed: #'Test1-TAG1'.
	packageExisting := self createNewPackageNamed: #Test1.
	class := self createNewClassNamed: 'TestClass' inPackage: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1'.
	self assert: package2 isNotNil.
	self assert: package2 equals: packageExisting.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'TAG1') classes includes: class)
]

{ #category : 'tests - demotion' }
RPackageTest >> testDemoteToRPackageNamedKeepOrganizer [

	| newOrganizer package renamedPackage |
	newOrganizer := RPackageOrganizer new.

	package := newOrganizer ensurePackage: #'Test1-TAG1'.

	renamedPackage := package demoteToTagInPackage.

	self assert: renamedPackage organizer identicalTo: newOrganizer
]

{ #category : 'tests - demotion' }
RPackageTest >> testDemoteToRPackageNamedMultilevelPackage [

	| package1 package2 class |
	package1 := self createNewPackageNamed: #'Test1-TAG1-X1'.
	class := self createNewClassNamed: 'TestClass' inPackage: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1-TAG1'.
	self assert: package2 isNotNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 classTagNamed: 'X1') classes includes: class)
]

{ #category : 'tests - demotion' }
RPackageTest >> testDemoteToRPackageNamedWithExtension [

	| packageOriginal packageDemoted class classOther |
	packageOriginal := self createNewPackageNamed: #'Test1-TAG1'.
	class := self createNewClassNamed: 'TestClass' inPackage: packageOriginal.
	class compile: 'foo ^42' classified: 'accessing'.

	classOther := self createNewClassNamed: 'TestClassOther' inCategory: 'XXXX'.
	classOther compile: 'bar ^42' classified: #'*Test1-TAG1'.

	packageOriginal demoteToTagInPackage.

	self deny: (self organizer hasPackage: packageOriginal).
	packageDemoted := self organizer packageNamed: 'Test1'.
	self assert: packageDemoted isNotNil.
	self assert: (packageDemoted classes includes: class).
	self assert: ((packageDemoted classTagNamed: 'TAG1') classes includes: class).
	self assert: (packageDemoted extensionMethods includes: classOther >> #bar).
	self assert: (classOther >> #bar) protocolName equals: '*Test1-TAG1'.
	self assert: (packageDemoted classes includesAll: {
				 class.
				 classOther })
]

{ #category : 'tests - properties' }
RPackageTest >> testHasProperty [
	| package testValue |

	testValue := Date today.
	package := self class package.

	package propertyAt: #testKeySelector put: testValue.
	self	assert: (package hasProperty: #testKeySelector).

	package removeProperty: #testKeySelector.
	self deny: (package hasProperty: #testKeySelector)
]

{ #category : 'tests - queries' }
RPackageTest >> testHierarchyRoots [

	| roots |
	roots := (self packageOrganizer packageNamed: #'RPackage-Tests') hierarchyRoots.
	roots := roots collect: [ :each | each name ].
	#( RPackageTestCase ) do: [ :each | roots includes: each ]
]

{ #category : 'tests' }
RPackageTest >> testIsTestPackage [

	|packages |
	packages := self createMockTestPackages.
	"Happy case: test package 'MockPackage-Tests' must contain -Tests suffix."
	self assert: packages first isTestPackage equals: true.

	"Package 'MockPackage2-tests' is not test package, since it has lowercase suffix."
	self assert: packages second isTestPackage  equals: false.

	"Happy case: regular package 'MockPackage' without -Tests suffix is not a test package."
	self assert: packages third isTestPackage  equals: false.

	"Package 'MockPackage-Tests-Package' containting -Tests- in middle, so it is test package."
	self assert: packages fourth isTestPackage equals: true.

	"cleanup of inst.vars should be done in tearDown"
]

{ #category : 'tests - MC' }
RPackageTest >> testMcPackage [

	| rPackage |
	rPackage := self organizer ensurePackage: #Test1.
	self assert: rPackage mcPackage equals: (MCPackage new name: #Test1)
]

{ #category : 'tests - MC' }
RPackageTest >> testMcWorkingCopy [

	| rPackage |
	rPackage := self organizer ensurePackage: #Test1.
	self assert: rPackage mcWorkingCopy identicalTo: (MCWorkingCopy forPackageNamed: #Test1)
]

{ #category : 'tests - properties' }
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

{ #category : 'tests' }
RPackageTest >> testRemoveEmptyTags [

	| package class tag1 tag2 |
	package := self createNewPackageNamed: #Test1.

	tag1 := package ensureTag: #Tag1.
	tag2 := package ensureTag: #Tag2.

	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-Tag1'.

	self assert: (tag1 includesClass: class).
	self deny: tag1 isEmpty.
	self assert: tag2 isEmpty.

	self assert: (package includesClassTagNamed: #Tag1).
	self assert: (package includesClassTagNamed: #Tag2).

	package removeEmptyTags.

	self assert: (package includesClassTagNamed: #Tag1).
	self deny: (package includesClassTagNamed: #Tag2)
]

{ #category : 'tests' }
RPackageTest >> testRemoveTag [

	| p1 a1 b1 |
	p1 := self createNewPackageNamed: #P1.

	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	self assert: p1 classTags size equals: 1. "We start with the root tag"

	p1 importClass: a1 inTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.

	p1 importClass: b1 inTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 removeTag: #bar.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 removeTag: #foo.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assertEmpty: (p1 classesTaggedWith: #foo)
]

{ #category : 'tests' }
RPackageTest >> testRemoveTagRemoveClasses [

	| p1 a1 |
	p1 := self createNewPackageNamed: #P1.

	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	self assert: p1 classTags size equals: 1. "We start with the root tag"

	p1 importClass: a1 inTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.

	p1 removeTag: #foo.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assertEmpty: (p1 classesTaggedWith: #foo).
	self deny: (p1 includesClass: a1).
	self assert: (self organizer packageOf: a1) name equals: RPackage defaultPackageName.
	self assert: a1 isObsolete
]

{ #category : 'tests' }
RPackageTest >> testRenamePackageAlsoRenameAllExtensionProtocols [
	"test that when we rename a category, all corresponding extension protocols in the system are renamed"

	| p1 p2 p3 classInY classInZ |
	p1 := self createNewPackageNamed: #Test1.
	p2 := self createNewPackageNamed: #Test2.
	p3 := self createNewPackageNamed: #Test3.

	classInY := self createNewClassNamed: 'ClassInYPackage' inPackage: p2.
	classInZ := self createNewClassNamed: 'ClassInZPackage' inPackage: p3.

	classInY compile: #extensionFromXInClassInY classified: '*' , p1 name.
	classInY compile: #longNameExtensionFromXInClassInY classified: '*' , p1 name , '-subcategory'.
	classInZ compile: #extensionFromXInClassInZ classified: '*' , p1 name.

	p1 renameTo: #NewPackageName.

	self assert: p1 name equals: #NewPackageName.
	self assert: (classInY >> #extensionFromXInClassInY) protocolName equals: '*' , p1 name.
	self assert: (classInY >> #longNameExtensionFromXInClassInY) protocolName equals: '*' , p1 name , '-subcategory'.
	self assert: (classInZ >> #extensionFromXInClassInZ) protocolName equals: '*' , p1 name
]

{ #category : 'tests' }
RPackageTest >> testRenameUpdateTheOrganizer [
	"test that when we rename a category, the organizer dictionary is update with this new name, so that we can access the package with this new name as key"

	| package |
	package := self createNewPackageNamed: #Test1.

	package renameTo: #Test2.
	self assert: package name equals: #Test2.
	self assert: (self organizer packageNamed: #Test2) identicalTo: package.
	self deny: (self organizer hasPackage: #Test1)
]
