"
RPackageOrganizerTest tests the behavior of PackageOrganizer. 
Note that it does not rely on announcement to be emitted to maintain invariants.
Therefore the new created PackageOrganizer is not registered to listen to event.


"
Class {
	#name : #RPackageOrganizerTest,
	#superclass : #RPackageTestCase,
	#category : #'RPackage-Tests'
}

{ #category : #utilities }
RPackageOrganizerTest class >> removeClassNamedIfExists: aClassNameSymbol [

	self environment at: aClassNameSymbol asSymbol ifPresent: [ :c | c removeFromSystem ]
]

{ #category : #utilities }
RPackageOrganizerTest class >> removePackageIfExist: aName [

	self packageOrganizer basicUnregisterPackageNamed: aName asSymbol
]

{ #category : #tests }
RPackageOrganizerTest >> p1Name [
	^ 'RPackageTestP1'
]

{ #category : #tests }
RPackageOrganizerTest >> p2Name [
	^ 'RPackageTestP2'
]

{ #category : #tests }
RPackageOrganizerTest >> p3Name [
	^ 'RPackageTestP3'
]

{ #category : #'tests - extending' }
RPackageOrganizerTest >> pointRectangleInGraphElement [

	^ (RPackage named: #GraphElement)
		  addClassDefinition: Point;
		  addMethod: Point >> #x;
		  addMethod: Point >> #rotateBy:about:;
		  addClassDefinition: Rectangle;
		  addMethod: self quadrangleClass >> #intersect:;
		  yourself
]

{ #category : #utilities }
RPackageOrganizerTest >> quadrangleClass [
	^ self organizer environment at: #QuadrangleForTesting
]

{ #category : #tests }
RPackageOrganizerTest >> testAccessingPackage [

	| p1 |
	p1 := self createNewPackageNamed: 'P1'.
	self assert: (self organizer packageNamed: #P1) equals: p1.
	self should: [ self organizer packageNamed: #P22 ] raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testCreateNewPackageWithConflictRaisesConflictException [

	self organizer createPackageNamed: 'P1'.
	self should: [ self organizer createPackageNamed: 'P1' ] raise: RPackageConflictError
]

{ #category : #tests }
RPackageOrganizerTest >> testCreateNewPackageWithoutConflictCreatesPackage [

	self organizer createPackageNamed: 'P1'.
	self assert: (self organizer hasPackage: 'P1')
]

{ #category : #tests }
RPackageOrganizerTest >> testDefinedClassesInstanceAndMetaSideAPI [

	| p1 |
	p1 := self createNewPackageNamed: self p1Name.
	self createNewClassNamed: #MyPoint inPackage: p1.
	self assert: self organizer packageNames size equals: 2.
	self assert: self organizer packages size equals: 2.
	self assert: (self organizer packageNamed: self p1Name) definedClasses size equals: 1
]

{ #category : #tests }
RPackageOrganizerTest >> testEmpty [

	self assert: self organizer packageNames size equals: 1.
	self assert: (self organizer packageNames includes: RPackage defaultPackageName)
]

{ #category : #tests }
RPackageOrganizerTest >> testExtensionMethodNotExactlyTheName [

	| p1 p2 c1 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	c1 := self createNewClassNamed: #C1 inPackage: p2.

	c1 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*p1-something'.
	self organizer addMethod: c1 >> #methodPackagedInP1.

	self deny: (self organizer hasPackage: #'p1-something').
	self assert: (p1 extendedClasses includes: c1)
]

{ #category : #tests }
RPackageOrganizerTest >> testFullRegistration [

	| p1 p2 p3 a1 a2 b1 b2 a3 |
	"taken from setup of RPackageReadOnlyCompleteSetup"

	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.

	a1 :=  self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	a2 := self createNewClassNamed: #A2DefinedInP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2DefinedInP2 inPackage: p2.
	a3 := self createNewClassNamed: #A3DefinedInP3 inPackage: p3.

	a1 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#methodDefinedInP1).
	a1 compileSilently: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p1 addMethod: (a1>>#anotherMethodDefinedInP1).

	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).

	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).

	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: (a2 class>>#classSideMethodDefinedInP3).

	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"

	self assert: (p2 includesClass: a2).
	"a locally defined class extended by other packages"

	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2)
]

{ #category : #tests }
RPackageOrganizerTest >> testHasPackage [

	| organizer packageFromOtherOrganizer name package |
	name := 'TestPackageNameForPackageOrganizer'.
	organizer := RPackageOrganizer new.
	packageFromOtherOrganizer := RPackageOrganizer new ensurePackage: name.
	self deny: (organizer hasPackage: name).
	self deny: (organizer hasPackage: packageFromOtherOrganizer).

	package := organizer ensurePackage: name.

	self assert: (organizer hasPackage: name).
	self assert: (organizer hasPackage: package).

	self deny: (organizer hasPackage: packageFromOtherOrganizer)
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageConflictWithPackage [

	self createNewPackageNamed: 'P1'.
	self should: [ (RPackage named: 'P1') register ] raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageConflictWithPackageTag [

	| package1 |
	self flag: #package. "This is a limitiation of the system organizer. I hope to kill this limitiation before p12 release."
	package1 := self createNewPackageNamed: 'P1'.
	package1 addClassTag: #T1.

	self should: [ self createNewPackageNamed: 'P1-T1' ] raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageTagConflictWithPackage [

	| package1 package2 |
	self flag: #package. "This is a limitiation of the system organizer. I hope to kill this limitiation before p12 release."
	package1 := self createNewPackageNamed: 'P1-T1'.

	package2 := self createNewPackageNamed: 'P1'.
	self should: [ package2 addClassTag: #T1 ] raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisteredNumberOfPackageIsOk [
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self organizer basicRegisterPackage: p1.
	self organizer basicRegisterPackage: p2.
	self organizer basicRegisterPackage: p3.
	self assert: self organizer packageNames size equals: 4.
	self organizer basicUnregisterPackageNamed: p3 name.
	self assert: self organizer packageNames size equals: 3
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisteredPackages [

	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self assert: self organizer packageNames size equals: 4. "We also have the default package."
	{ p1 . p2 . p3 } do: [ :package |
		self assert: (self organizer packageNames includes: package name).
		self assert: (self organizer packages includes: package) ]
]

{ #category : #'tests - extending' }
RPackageOrganizerTest >> testRegistrationExtendingPackages [

	| p |
	self createNewClassNamed: 'QuadrangleForTesting' inCategory: self class category.
	self quadrangleClass compileSilently: 'intersect:aPoint ^ false'.
	self assertEmpty: (self organizer extendingPackagesOf: self quadrangleClass).
	p := self pointRectangleInGraphElement.
	self organizer registerExtendingPackage: p forClass: self quadrangleClass.
	self denyEmpty: (self organizer extendingPackagesOf: self quadrangleClass).
	self assert: (self organizer extendingPackagesOf: self quadrangleClass) anyOne name equals: #GraphElement
]

{ #category : #tests }
RPackageOrganizerTest >> testRemoveEmptyPackagesAndTags [

	| organizer package1 package2 package3 package4 class tag1 tag2 |
	"This one will contain a class"
	package1 := (self createNewPackageNamed: #Test1).
	"This one will contain an extension method"
	package2 := (self createNewPackageNamed: #Test2).
	"This one will contain a tag with a class and an empty tag"
	package3 := (self createNewPackageNamed: #Test3).
	"This one will be empty"
	package4 := (self createNewPackageNamed: #Test4).

	tag1 := package3 addClassTag: #Tag1.
	tag2 := package3 addClassTag: #Tag2.

	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1'.
	class compile: 'extension ^ 1' classified: '*Test2'.
	
	class := self createNewClassNamed: 'TestClass2' inCategory: 'Test3-Tag1'.

	self flag: #package. "Use the organizer provided by the tests later."
	organizer := package1 organizer.

	self assert: (organizer hasPackage: package1).
	self deny: package1 isEmpty.
	self assert: (organizer hasPackage: package2).
	self deny: package2 isEmpty.
	self assert: (organizer hasPackage: package3).
	self deny: package3 isEmpty.
	self deny: tag1 isEmpty.
	self assert: tag2 isEmpty.
	self assert: (package3 includesClassTagNamed: #Tag1).
	self assert: (package3 includesClassTagNamed: #Tag2).
	self assert: (organizer hasPackage: package4).
	self assert: package4 isEmpty.

	organizer removeEmptyPackagesAndTags.

	self assert: (organizer hasPackage: package1).
	self assert: (organizer hasPackage: package2).
	self assert: (organizer hasPackage: package3).
	self assert: (package3 includesClassTagNamed: #Tag1).
	self deny: (package3 includesClassTagNamed: #Tag2).
	self deny: (organizer hasPackage: package4)
]

{ #category : #tests }
RPackageOrganizerTest >> testRemovePackage [

	| p1 p2 p3 a1 a2 b1 b2 a3 |
	"taken from setup of RPackageReadOnlyCompleteSetup"
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.

	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	a2 := self createNewClassNamed: #A2DefinedInP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2DefinedInP2 inPackage: p2.
	a3 := self createNewClassNamed: #A3DefinedInP3 inPackage: p3.

	a1 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: a1 >> #methodDefinedInP1.
	a1 compileSilently: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p1 addMethod: a1 >> #anotherMethodDefinedInP1.

	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: a2 >> #methodDefinedInP1.

	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: a2 >> #methodDefinedInP2.

	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: a2 >> #methodDefinedInP3.

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: a2 class >> #classSideMethodDefinedInP3.

	self organizer removePackage: p1.
	self organizer removePackage: p2.
	self organizer removePackage: p3.

	self deny: (self organizer packageOf: a1) isNil.
	self deny: (self organizer packageOf: a2) isNil.
	self deny: (self organizer packageOf: b1) isNil.
	self deny: (self organizer packageOf: b2) isNil.
	self deny: (self organizer packageOf: a3) isNil
]

{ #category : #tests }
RPackageOrganizerTest >> testTestPackageNames [

	| packages |
	packages := self createMockTestPackages.

	"Only 2 mock package names are test packages:  'MockPackage-Tests' 'MockPackage-Tests-Package'"
	self assert: self organizer testPackageNames size equals: 2.

	"Names of test packages are symbols."
	self assert: (self organizer testPackageNames allSatisfy: #isSymbol)
]

{ #category : #tests }
RPackageOrganizerTest >> testTestPackages [

	| packages |
	packages := self createMockTestPackages.

	"Only 2 mock packages are test packages:  'MockPackage-Tests' 'MockPackage-Tests-Package'."
	self assert: self organizer testPackages size equals: 2.

	"all items from resulting collection are test packages."
	self assert: (self organizer testPackages allSatisfy: #isTestPackage)
]

{ #category : #tests }
RPackageOrganizerTest >> testUnregisterBasedOnNames [

	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self assert: self organizer packageNames size equals: 4.

	{p1 . p2 . p3} do: [:each |
		(self organizer basicUnregisterPackageNamed: each name).
		self deny: (self organizer packageNames includes: each name) ].

	self assert: self organizer packageNames size equals: 1
]

{ #category : #'tests - extending' }
RPackageOrganizerTest >> testUnregistrationExtendingPackages [

	| p |
	self createNewClassNamed: 'QuadrangleForTesting' inCategory: self class category.
	self quadrangleClass compileSilently: 'intersect:aPoint ^ false'.
	p := self pointRectangleInGraphElement.
	self organizer registerExtendingPackage: p forClass: self quadrangleClass.
	self denyEmpty: (self organizer extendingPackagesOf: self quadrangleClass).
	self assert: (self organizer extendingPackagesOf: self quadrangleClass) anyOne name equals: #GraphElement.
	self organizer unregisterExtendingPackage: p forClass: self quadrangleClass.
	self assertEmpty: (self organizer extendingPackagesOf: self quadrangleClass)
]
