"
The tests takes as fixture the following situation (which is constructed partially)
For tests that do not need to check the incremental construction.


P1 
	A1DefinedInP1>>methodDefinedInP1
	B1DefinedInP1	
	A2DefinedInP2>>methodDefinedInP1  (was *P2)
	
P2
	A2DefinedInP2>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInP3	
	A2DefinedInP2>>methodDefinedInP3  (was *P2*)
"
Class {
	#name : #RPackageIncrementalTest,
	#superclass : #RPackageTestCase,
	#category : #'RPackage-Tests'
}

{ #category : #utilities }
RPackageIncrementalTest >> p1Name [
	^ 'RPackageTestP1'
]

{ #category : #utilities }
RPackageIncrementalTest >> p2Name [
	^ 'RPackageTestP2'
]

{ #category : #utilities }
RPackageIncrementalTest >> p3Name [
	^ 'RPackageTestP3'
]

{ #category : #utilities }
RPackageIncrementalTest >> removeClassNamedIfExists: aClassNameSymbol [
	testingEnvironment at: aClassNameSymbol asSymbol ifPresent: [:c| c removeFromSystem]
]

{ #category : #running }
RPackageIncrementalTest >> setUp [
	super setUp.
	Author fullName ifNil: [Author fullName: 'RPackage']
]

{ #category : #running }
RPackageIncrementalTest >> tearDown [

	createdPackages do: [ :each | self removePackage: each name ].
	"just remove package from package organizer dictionary"

	createdPackages do: [ :each |
		self allWorkingCopies
			detect: [ :mcPackage | mcPackage packageName = each packageName asString ]
			ifFound: [ :mcPackage | mcPackage unregister ].
		each extendedClasses do: [ :extendedClass | self organizer unregisterExtendingPackage: each forClass: extendedClass ] ].
	"all ***extending*** classes the packages are also unregistered from PackageOrganizer"
	super tearDown
]

{ #category : #'tests - method addition removal' }
RPackageIncrementalTest >> testAddRemoveMethod [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.

	p2 addMethod: a2 >> #methodDefinedInP2.
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: a2 >> #methodDefinedInP1.
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: a2 >> #methodDefinedInP3.

	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2).

	"removeMethod by default removes from defined methods and not extension"
	p2 removeMethod: a2 >> #methodDefinedInP2.
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2).

	p1 removeMethod: a2 >> #methodDefinedInP1.
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofClass: a2)
]

{ #category : #'tests - method addition removal' }
RPackageIncrementalTest >> testAddRemoveSelector [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.

	p2 addMethod: a2 >> (a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2').
	p1 addMethod: a2 >> (a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1').
	p3 addMethod: a2 >> (a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3').

	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2).

	p2 removeMethod: a2 >> #methodDefinedInP2.
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2).

	p1 removeMethod: a2 >> #methodDefinedInP1.
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofClass: a2)
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testClassAddition [

	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1.
	self assertEmpty: p definedClasses.
	p importClass: a1.
	self assert: p definedClasses size equals: 1.
	self assert: (p includesClass: a1).
	self assert: (p includesClass: a1 class)
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testClassDefinitionRemoval [

	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1.
	b1 := self createNewClassNamed: #B1InPAckageP1.
	self assertEmpty: p definedClasses.

	p importClass: a1.
	p importClass: b1.
	self assert: p definedClasses size equals: 2.

	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).

	p removeClass: a1.
	self assert: p definedClasses size equals: 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).

	p removeClass: b1.
	self deny: (p includesClass: b1)
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testClassDefinitionWithTagsRemoval [

	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.

	a1 := self createNewClassNamed: #A1InPAckageP1 inPackage: p.
	b1 := self createNewClassNamed: #B1InPAckageP1 inPackage: p.
	self assert: p definedClasses size equals: 2.

	p importClass: a1 inTag: 'a1-tag'.
	p importClass: b1 inTag: 'b1-tag'.
	self assert: p classTags size equals: 2.

	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).

	p removeClass: a1.
	self assert: p definedClasses size equals: 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).

	p removeClass: b1.
	self deny: (p includesClass: b1).
	self assert: p classTags size equals: 0
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testDefinedClassesAndDefinedClassNames [

	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1 inPackage: p.
	self assert: p definedClasses size equals: 1.
	self assert: (p definedClasses includes: a1).
	self assert: (p definedClassNames includes: a1 name).

	b1 := self createNewClassNamed: #B1InPackageP1 inPackage: p.
	self assert: p definedClasses size equals: 2.
	self assert: (p definedClasses includes: b1).
	self assert: (p definedClassNames includes: b1 name)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testExtensionClassNames [

	| p1 p2 a2 b2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: a2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 1.
	self assert: p1 extensionMethods size equals: 1.
	self assert: (p1 extendedClassNames includes: #A2InPackageP2).
	self deny: (p1 includesClass: a2). "method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 2.
	self assert: p1 extensionMethods size equals: 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 3.
	self assert: p1 extensionMethods size equals: 3.
	self assert: p1 extendedClasses size equals: 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testExtensionClasses [

	| p1 p2 a2 b2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).

	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.

	self assert: p1 extendedClasses size equals: 1.
	self assert: (p1 extendedClasses includes: a2).
	self assert: p1 extendedClassNames size equals: 1.
	self assert: (p1 extendedClassNames includes: a2 name).

	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.

	self assert: p1 extendedClasses size equals: 2.
	self assert: (p1 extendedClasses includes: b2 class).
	"extensionClasses returns or metaclasses while extensionClassNames returns class names (but not metaclass names)"

	self assert: p1 extendedClassNames size equals: 2.
	self assert: (p1 extendedClassNames includes: b2 name)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testExtensionClassesWithCompiledMethod [

	| p1 p2 a2 b2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: b2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.

	self assert: p1 extensionMethods size equals: 1.
	self assert: p1 extensionSelectors size equals: 1.
	self assert: p1 extendedClasses size equals: 1.
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 2.
	self assert: p1 extensionMethods size equals: 2.
	self assert: p1 extendedClasses size equals: 2.
	self deny: (p1 includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 3.
	self assert: p1 extensionMethods size equals: 3.
	self assert: p1 extendedClasses size equals: 2.
	self deny: (p1 includesClass: b2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testExtensionMethods [

	| p1 p2 a2 b2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.

	self assert: p1 extensionSelectors size equals: 1.
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , p1 name.
	self assert: p1 extensionSelectors size equals: 2
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testImportClassNoDuplicate [

	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1.
	self assertEmpty: p definedClasses.
	p importClass: a1.
	self assert: p definedClasses size equals: 1.
	b1 := self createNewClassNamed: #B1InPackageP1.
	p importClass: a1.
	"adding the same class does not do anything - luckily"
	self assert: p definedClasses size equals: 1.
	p importClass: b1.
	self assert: p definedClasses size equals: 2
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludeClass [

	| p1 p2 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.

	self deny: (p1 includesClass: a2).
	p1 addMethod: a2 >> #methodPackagedInP1.
	self deny: (p1 includesClass: a2).
	"We should declare the class explictly. Adding a method does not declare
	the class as defined. The reason is that like that the client controls the granularity
	and moment of class registration."

	p1 importClass: a2.
	self assert: (p1 includesClass: a2).
	self assert: (p1 includesClassNamed: a2 name)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludeClassMore [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , p3 name.

	self assert: (p2 includesClass: a2).
	self deny: (p1 includesClass: a2).
	self deny: (p3 includesClass: a2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludeSelectorOfClass [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , p3 name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2).

	self deny: (p2 includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).

	self deny: (p2 includesSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludeSelectorOfMetaClass [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 class compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 class compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.
	a2 class compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , p3 name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a2 class).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2 class).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a2 class).

	self deny: (p2 includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2 class).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClass: a2 class).

	self deny: (p2 includesSelector: #methodDefinedInP1 ofClass: a2 class).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2 class).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2 class).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludesMethodOfClassInPresenceOfOtherPackageExtensions [

	| p1 p2 p3 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.

	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2).

	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , p3 name.
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).

	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testIncludesOrTouches [

	| p1 p2 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).

	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.

	self assert: p1 extensionMethods size equals: 1.
	self assert: p1 extensionSelectors size equals: 1.
	self deny: (p1 includesClass: a2).
	self assert: (p1 definesOrExtendsClass: a2).
	self assert: (p2 includesClass: a2)
]

{ #category : #'tests - method addition removal' }
RPackageIncrementalTest >> testMethodAddition [

	| p1 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	a1 compileSilently: 'foo ^ 10'.
	p1 addMethod: a1 >> #foo.
	self assert: (p1 includesSelector: #foo ofClass: a1)
]

{ #category : #'tests - method addition removal' }
RPackageIncrementalTest >> testMethodPackageResolution [

	| p1 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A2InPackageP1 inPackage: p1.
	a1 compile: 'method ^ #methodDefinedInP1'.
	a1 class compile: 'method ^ #methodDefinedInP1'.

	self assert: (a1 >> #method) package identicalTo: p1.
	self assert: (a1 class >> #method) package identicalTo: p1
]

{ #category : #'tests - package belonging' }
RPackageIncrementalTest >> testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt [

	| p1 p2 a2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.

	self assert: a2 package equals: p2.
	p1 extensionMethods do: [ :each | "the package of a class which is extended inside a package p, is not p
	but the package where the class was defined"
		self deny: each methodClass package equals: p1 ]
]

{ #category : #'tests - package belonging' }
RPackageIncrementalTest >> testPackageOfClassForDefinedClasses [

	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inPackage: p.
	b1 := self createNewClassNamed: #B1InPAckageP1 inPackage: p.

	self assert: a1 package equals: p.
	self assert: b1 package equals: p
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testRemoveClassRemovesExtensions [

	| p1 p2 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inPackage: p1.
	self assert: p1 definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , p2 name.

	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeFromSystem.

	self deny: (p2 includesSelector: #methodDefinedInP2 ofClass: a1).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a1).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testRemoveExtensionMethodRemovesExtensionsFromPackage [

	| p1 p2 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inPackage: p1.
	self assert: p1 definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , p2 name.

	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeSelector: #methodDefinedInP2.

	self deny: (p2 includesSelector: #methodDefinedInP2 ofClass: a1).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClass: a1).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : #'tests - extension' }
RPackageIncrementalTest >> testTwoClassesWithExtensions [

	| p1 p2 a2 b2 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.
	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , p1 name.

	self assert: p1 classes size equals: 2.
	self assert: p2 classes size equals: 2
]

{ #category : #'tests - class addition removal' }
RPackageIncrementalTest >> testUniqueClassInDefinedClassesUsingAddClassDefinition [

	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1.
	self assertEmpty: p definedClasses.
	p importClass: a1.
	self assert: p definedClasses size equals: 1.
	p importClass: a1.
	self assert: p definedClasses size equals: 1.
	p importClass: a1 class.
	self assert: p definedClasses size equals: 1
]
