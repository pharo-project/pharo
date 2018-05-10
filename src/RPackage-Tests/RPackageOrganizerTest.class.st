"
RPackageOrganizerTest tests the behavior of PackageOrganizer. 
Note that it does not rely on announcement to be emitted to maintain invariants.
Therefore the new created PackageOrganizer is not registered to listen to event.


"
Class {
	#name : #RPackageOrganizerTest,
	#superclass : #RPackageTestCase,
	#instVars : [
		'packageOrganizer',
		'previousAuthor',
		'quadrangleClass'
	],
	#category : #'RPackage-Tests'
}

{ #category : #utilities }
RPackageOrganizerTest class >> removeClassNamedIfExists: aClassNameSymbol [

	Smalltalk globals at: aClassNameSymbol asSymbol ifPresent: [ :c | c removeFromSystem ]
]

{ #category : #utilities }
RPackageOrganizerTest class >> removePackageIfExist: aName [

	self packageOrganizer basicUnregisterPackageNamed: aName asSymbol.
		
	
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

{ #category : #utilities }
RPackageOrganizerTest >> packageOrganizer [

	^ packageOrganizer 
]

{ #category : #'tests extending' }
RPackageOrganizerTest >> pointRectangleInGraphElement [ 
	| p |
	p := self packageClass named: #GraphElement.
	p addClassDefinition: Point.
	p addMethod: Point>>#x.
	p addMethod: Point>>#rotateBy:about:. 
	p addClassDefinition: Rectangle.
	p addMethod: self quadrangleClass >>#intersect:.
	^ p
]

{ #category : #utilities }
RPackageOrganizerTest >> quadrangleClass [
	^ self class environment at: #QuadrangleForTesting 
]

{ #category : #running }
RPackageOrganizerTest >> setUp [
	super setUp.
	createdClasses := OrderedCollection new.
	createdPackages := OrderedCollection new.
	packageOrganizer := self packageOrganizerClass basicNew initialize.
	packageOrganizer debuggingName: 'For PackageOrganizerTest'.
	previousAuthor := Author fullName ifNil: [Author fullName: 'RPackage'].
	
	self createNewClassNamed: 'QuadrangleForTesting' inCategory: self class category.
	self quadrangleClass compileSilently: 'intersect:aPoint ^ false'.
]

{ #category : #'tests extending' }
RPackageOrganizerTest >> stringCharacterInCollPackage [
	| p |
	p := self packageClass named: #CollPackage.
	p addClassDefinition: String.
	p addClassDefinition: Character.
	p addMethod: String>>#indexOf:.
	p addMethod: String>>#isAllDigits.
	p addMethod: String class>>#cr.
	p addMethod: Character>>#asInteger.
	p addMethod: Text>>#allBold.
	^ p
]

{ #category : #running }
RPackageOrganizerTest >> tearDown [ 
	
	Author fullName: previousAuthor.
	
	(createdClasses reject: [:c| c isObsolete])
		do: [:cls| 
				"(self packageClass organizer includesClass: cls)
					ifTrue: [self packageOrganizer unregisterClass: cls.]."
				cls removeFromSystem ].
	"createdPackages do:
		[:each | self packageOrganizer unregisterPackage: each]"
	super tearDown 
	

]

{ #category : #tests }
RPackageOrganizerTest >> testAccessingPackage [	

	| p1 |
	p1 := self createNewPackageNamed: 'P1'.
	self packageOrganizer registerPackage: p1.
	p1 addClassDefinition: Point.
	p1 addMethod: Point>>#x.
	p1 addMethod: Point class>>#x:y:. 
	self assert: (self packageOrganizer packageNamed: #P1) equals: p1.
	self should: [(self packageOrganizer packageNamed: #P22)] raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testCreateNewPackageWithConflictRaisesConflictException [

	packageOrganizer createPackageNamed: 'P1'.
	self
		should: [ packageOrganizer createPackageNamed: 'P1' ]
		raise: RPackageConflictError
]

{ #category : #tests }
RPackageOrganizerTest >> testCreateNewPackageWithoutConflictCreatesPackage [

	packageOrganizer createPackageNamed: 'P1'.
	self assert: (packageOrganizer includesPackageNamed: 'P1')
]

{ #category : #tests }
RPackageOrganizerTest >> testDefinedClassesInstanceAndMetaSideAPI [
	
	| p1 |
	p1 := self createNewPackageNamed: 'P1'.
	self packageOrganizer registerPackage: p1.
	p1 addClassDefinition: Point.
	p1 addMethod: Point>>#x.
	p1 addMethod: Point class>>#x:y:. 
	self assert: self packageOrganizer packageNames size equals: 2.
	self assert: self packageOrganizer packages size equals: 2.
	self assert: (self packageOrganizer packageNamed: #P1) definedClasses size equals: 1
]

{ #category : #tests }
RPackageOrganizerTest >> testEmpty [
	
	self assert: self packageOrganizer packageNames size equals: 1.
	self assert: (self packageOrganizer packageNames includes: self packageClass defaultPackageName)

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
	

	self packageOrganizer registerPackage: p1.
	self packageOrganizer registerPackage: p2.
	self packageOrganizer registerPackage: p3.
	
	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"
	
	self assert: (p2 includesClass: a2).  
	"a locally defined class extended by other packages"
	
	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2).

]

{ #category : #tests }
RPackageOrganizerTest >> testFullUnregistration [

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
	

	self packageOrganizer registerPackage: p1.
	self packageOrganizer registerPackage: p2.
	self packageOrganizer registerPackage: p3.
	
	self packageOrganizer unregisterPackage: p1.
	self packageOrganizer unregisterPackage: p2.
	self packageOrganizer unregisterPackage: p3.
	

	self deny: (self packageOrganizer includesPackageBackPointerForClass: a1).
	self deny: (self packageOrganizer includesPackageBackPointerForClass: a2).
	self deny: (self packageOrganizer includesPackageBackPointerForClass: b1).
	self deny: (self packageOrganizer includesPackageBackPointerForClass: b2).
	self deny: (self packageOrganizer includesPackageBackPointerForClass: a3).

]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageConflictWithPackage [

	| package1 package2 |
	package1 := self createNewPackageNamed: 'P1'.
	package1 register.
	
	package2 := self packageClass named: 'P1'.
	self 
		should: [ package2 register ]
		raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageConflictWithPackageTag [

	| package1 package2 |

	package1 := self createNewPackageNamed: 'P1'.
	package1 register.
	package1 addClassTag: #T1.
	
	package2 := self createNewPackageNamed: 'P1-T1'.
	self 
		should: [ package2 register ]
		raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisterPackageTagConflictWithPackage [
	| package1 package2 |

	package1 := self createNewPackageNamed: 'P1-T1'.
	package1 register.	
	
	package2 := self createNewPackageNamed: 'P1'.
	self 
		should: [ package2 addClassTag: #T1 ]
		raise: Error
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisteredIsIncludedInPackageNames [	
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer registerPackage: p1.
	self packageOrganizer registerPackage: p2.
	self packageOrganizer registerPackage: p3.
	self assert: self packageOrganizer packageNames size equals: 4.
	self assert: (self packageOrganizer includesPackage: p1). 
	self assert: (self packageOrganizer includesPackageNamed: p1 name). 
	{p1 . p2 . p3} do: [:each | self assert:  (self packageOrganizer packageNames includes: each name)]


]

{ #category : #tests }
RPackageOrganizerTest >> testRegisteredIsThere [	
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer basicRegisterPackage: p1.
	self packageOrganizer basicRegisterPackage: p2.
	self packageOrganizer basicRegisterPackage: p3.
	self assert: self packageOrganizer packageNames size equals: 4.
	
	{p1 . p2 . p3} do: [:each | 
		self assert:  (self packageOrganizer packageNames includes: each name)]
]

{ #category : #tests }
RPackageOrganizerTest >> testRegisteredNumberOfPackageIsOk [
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer basicRegisterPackage: p1.
	self packageOrganizer basicRegisterPackage: p2.
	self packageOrganizer basicRegisterPackage: p3.
	self assert: self packageOrganizer packageNames size equals: 4.
	self packageOrganizer basicUnregisterPackage: p3.
	self assert: self packageOrganizer packageNames size equals: 3.
]

{ #category : #'tests extending' }
RPackageOrganizerTest >> testRegistrationExtendingPackages [
	| p |
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	p := self pointRectangleInGraphElement.
	(self packageOrganizer
					registerExtendingPackage:  p
					forClass: self quadrangleClass).
	self deny: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) anyOne name equals: #GraphElement.
	
]

{ #category : #'tests extensions' }
RPackageOrganizerTest >> testSilentlyRenameCategoryToBe [

	| class |
	
	class := self createNewClassNamed: #ClassForTestAboutSilentRecategorization inCategory: 'RPackage-Tests'.
	class organization addCategory:#foo.
	self assert: (class organization categories includes: #foo).
	self deny: (class organization categories includes: #bar).
	class organization silentlyRenameCategory: #foo toBe: #bar.
	
	self assert: (class organization categories includes: #bar).
	self deny: (class organization categories includes: #foo).	
]

{ #category : #tests }
RPackageOrganizerTest >> testUnregisterBasedOnNames [
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer basicRegisterPackage: p1.
	self packageOrganizer basicRegisterPackage: p2.
	self packageOrganizer basicRegisterPackage: p3.
	self assert: self packageOrganizer packageNames size equals: 4.

	{p1 . p2 . p3} do: [:each | 
		(self packageOrganizer basicUnregisterPackageNamed: each name).
		self deny:  (self packageOrganizer packageNames includes: each name)]
]

{ #category : #'tests extending' }
RPackageOrganizerTest >> testUnregistrationExtendingPackages [
	| p |	
	p := self pointRectangleInGraphElement.
	self packageOrganizer
		registerExtendingPackage: p
		forClass: self quadrangleClass.
	self deny: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) anyOne name equals: #GraphElement.
	self packageOrganizer
					unregisterExtendingPackage:  p
					forClass: self quadrangleClass.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	
]
