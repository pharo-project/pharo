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
	Smalltalk at: aClassNameSymbol asSymbol ifPresent: [:c| c removeFromSystem]
]

{ #category : #running }
RPackageIncrementalTest >> setUp [
	super setUp.
	Author fullName ifNil: [Author fullName: 'RPackage'].
]

{ #category : #running }
RPackageIncrementalTest >> tearDown [
	
	| logging |
	createdPackages  do: [:each | self removePackage: each name].
	"just remove package from package organizer dictionary"
	
	createdPackages  do: [:each |
		|mCPackage|
		mCPackage := self allManagers 
							detect: [:mcPackage | mcPackage packageName = each packageName asString] 
							ifNone: [nil].
		mCPackage ifNotNil: [mCPackage unregister].
		each extendedClasses do: [ :extendedClass|
			self packageClass organizer
		 		unregisterExtendingPackage: each forClass: extendedClass.]].
	"all ***extending*** classes the packages are also unregistered from PackageOrganizer"									
	(createdClasses reject: [:c| c isObsolete]) do: [:cls| 
		"(RPackage organizer includesPackageBackPointerForClass: cls) 
			ifTrue: [cls package unregisterClass: cls.]. 
		when RPackageOrganizer was not looking at system event we had to do the commented actions"
		logging := false.
		cls removeFromSystem: logging.  
		"not logging so no event are raised"
		"but this also means that the consistency cannot be ensured by internal system announcer too."
		].					
	createdCategories do: [:each | 
		SystemOrganization removeCategory: each.
		 ].
	super tearDown.
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testAddClassAfterMethods [
	| p1 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p1 definedClasses size equals: 0.
	
	"now we add a new method to the class and the to the package: it is considered
	as class extension"
	
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#newlyAddedToA1).
	self assert: (p1 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	"Now we add the class and this means that the method is not an extension anymore but 
	a defined method"
	
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size equals: 1.
	
	self deny: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	
"
	self assert: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	self assert: (p3 includesSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
	"
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testAddClassAfterMethodsAtMetaclassToo [
	| p1 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p1 definedClasses size equals: 0.
	
	"now we add a new method to the class and the to the package: it is considered
	as class extension"
	
	a1 class compileSilently: 'newlyAddedToA1Class ^ #methodDefinedInP1'.
	p1 addMethod: (a1 class>>#newlyAddedToA1Class).
	self assert: (p1 includesSelector: #newlyAddedToA1Class ofClass: a1 class).
	self assert: (p1 includesExtensionSelector: #newlyAddedToA1Class ofClass: a1 class).
	self deny: (p1 includesDefinedSelector: #newlyAddedToA1Class ofClass: a1 class).
	
	"Now we add the class and this means that the method is not an extension anymore but 
	a defined method"
	
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size equals: 1.
	
	self deny: (p1 includesExtensionSelector: #newlyAddedToA1Class ofClass: a1 class).
	self assert: (p1 includesDefinedSelector: #newlyAddedToA1Class ofClass: a1 class).
	
	
"
	self assert: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	self assert: (p3 includesSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
	"
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testAddClassDefinitionNoDuplicate [
	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	b1 := self createNewClassNamed: #B1InPackageP1 inCategory: self p1Name.
	p addClassDefinition: a1.
	"adding the same class does not do anything - luckily"
	self assert: p definedClasses size equals: 1.
	p addClassDefinition: b1.
	self assert: p definedClasses size equals: 2.

]

{ #category : #'test method addition removal' }
RPackageIncrementalTest >> testAddRemoveMethod [
	| p1 p2 p3 a2   a2Name |
	a2Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: a2Name inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	
	p2 addMethod: (a2>>#methodDefinedInP2).
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2Name).
	
	"removeMethod by default removes from defined methods and not extension"
	p2 removeMethod: (a2>>#methodDefinedInP2).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	
	p1 removeMethod: (a2>>#methodDefinedInP1).
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).
]

{ #category : #'test method addition removal' }
RPackageIncrementalTest >> testAddRemoveSelector [
	| p1 p2 p3 a2   a2Name |
	a2Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: a2Name inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addSelector: #methodDefinedInP2 ofClassName: a2Name.
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addSelector: #methodDefinedInP1 ofClassName: a2Name.
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addSelector: #methodDefinedInP3 ofClassName: a2Name.
	
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2Name).
	
	p2 removeSelector: #methodDefinedInP2 ofClassName:  a2Name.
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	
	p1 removeSelector: #methodDefinedInP1 ofClassName:  a2Name.
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).
]

{ #category : #'test method addition removal' }
RPackageIncrementalTest >> testAddRemoveSelectorOfMetaclass [
	| p1 p2 p3 a2   a2Name a2class |
	a2Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: a2Name inPackage: p2.
	a2class := a2 class.
	a2class compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addSelector: #methodDefinedInP2 ofMetaclassName: a2Name.
	a2class compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addSelector: #methodDefinedInP1 ofMetaclassName: a2Name.
	a2class compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addSelector: #methodDefinedInP3 ofMetaclassName: a2Name.
	
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofMetaclassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofMetaclassName: a2Name).
	
	p2 removeSelector: #methodDefinedInP2 ofMetaclassName:  a2Name.
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	
	p1 removeSelector: #methodDefinedInP1 ofMetaclassName:  a2Name.
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testBogusClassAddition [
	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinitionName: a1 name.
	self assert: p definedClasses size equals: 1.
	self assert: (p includesClass: a1).
	self should: [p addClassDefinitionName: a1 class name] raise: Error

]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testClassAddition [
	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	self assert: (p includesClass: a1).
	self assert: (p includesClass: a1 class)

]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testClassDefinitionRemoval [
	| p a1 b1|
	
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	b1 := self createNewClassNamed: #B1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	
	p addClassDefinition: a1.
	p addClassDefinition: b1.
	self assert: p definedClasses size equals: 2.
	
	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: b1 class.
	self deny: (p includesClass: b1).

]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testClassDefinitionRemovalName [
	| p a1 b1|
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	b1 := self createNewClassNamed: #B1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinitionName: a1 name.
	p addClassDefinitionName: b1 name.
	self assert: p definedClasses size equals: 2.
	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).
	p removeClassDefinitionName: a1 name.
	self assert: p definedClasses size equals: 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).
	self should: [p removeClassDefinitionName: b1 class name] raise: Error.
	p removeClassDefinitionName: b1 name.
	self deny: (p includesClass: b1).

]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testClassDefinitionWithTagsRemoval [
	| p a1 b1|
	
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	b1 := self createNewClassNamed: #B1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	
	p addClassDefinition: a1.
	p addClassDefinition: b1.
	self assert: p definedClasses size equals: 2.
	
	p addClassDefinition: a1 toClassTag: 'a1-tag'. 
	p addClassDefinition: b1 toClassTag: 'b1-tag'.
	self assert: p classTags size equals: 2.
	
	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: b1 class.
	self deny: (p includesClass: b1).
	self assert: p classTags size equals: 0.

]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testDefinedClassesAndDefinedClassNames [
	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	self assert: (p definedClasses  includes: a1).
	self assert: (p definedClassNames includes: a1 name).
	
	b1 := self createNewClassNamed: #B1InPackageP1 inCategory: self p1Name.
	p addClassDefinition: b1.
	self assert: p definedClasses size equals: 2.
	self assert: (p definedClasses includes: b1).
	self assert: (p definedClassNames includes: b1 name)
	
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testExtensionClassNames [
	| p1 p2  a2 b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addSelector: #methodDefinedInP1 ofClassName: a2 name.
	
	self assert: p1 extensionSelectors size equals: 1.
	self assert: p1 extensionMethods size equals: 1.
	self assert: (p1 extendedClassNames includes: #A2InPackageP2).
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"
	
	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1'.
	p1 addSelector: #firstMethodInB2PackagedInP1 ofClassName: b2 name.
	
	self assert: p1 extensionSelectors size equals: 2.
	self assert: p1 extensionMethods size equals: 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2).
	
	b2 compileSilently: 'secondMethodInB2PackagedInP1 ^ 2'.
	p1 addSelector: #secondMethodInB2PackagedInP1 ofClassName: b2 name.
	
	self assert: p1 extensionSelectors size equals: 3.
	self assert: p1 extensionMethods size equals: 3.
	self assert: p1 extendedClasses size equals: 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2).
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testExtensionClasses [
	| p1 p2 a2  b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	
	self assert: p1 extendedClasses size equals: 1.
	self assert: (p1 extendedClasses  includes: a2).
	self assert: p1 extendedClassNames size equals: 1.
	self assert: (p1 extendedClassNames  includes: a2 name).
	
	b2 class compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (b2 class>>#methodPackagedInP1).
	
	self assert: p1 extendedClasses size equals: 2.
	self assert: (p1 extendedClasses  includes: b2 class).
	"extensionClasses returns or metaclasses while extensionClassNames returns class names (but not metaclass names)"

	self assert: p1 extendedClassNames size equals: 2.
	self assert: (p1 extendedClassNames  includes: b2 name).
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testExtensionClassesWithCompiledMethod [
	| p1 p2  a2 b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: b2).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	self assert: p1 extensionMethods size equals: 1.
	self assert: p1 extensionSelectors size equals: 1.
	self assert: p1 extendedClasses size equals: 1.
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"
	
	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1'.
	p1 addMethod: (b2>>#firstMethodInB2PackagedInP1).
	
	self assert: p1 extensionSelectors size equals: 2.
	self assert: p1 extensionMethods size equals: 2.
	self assert: p1 extendedClasses size equals: 2.
	self deny: (p1 includesClass: b2).
	
	b2 compileSilently: 'secondMethodInB2PackagedInP1 ^ 2'.
	p1 addMethod: (b2>>#secondMethodInB2PackagedInP1).
	
	self assert: p1 extensionSelectors size equals: 3.
	self assert: p1 extensionMethods size equals: 3.
	self assert: p1 extendedClasses size equals: 2.
	self deny: (p1 includesClass: b2).
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testExtensionMethods [	
	| p1 p2 a2  b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	self assert: p1 extensionSelectors size equals: 1.
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"
	
	b2 compileSilently: 'firstMethodInB2PackagedInP1 ^ 1'.
	p1 addMethod: (b2>>#firstMethodInB2PackagedInP1).
	self assert: p1 extensionSelectors size equals: 2.
	
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludeClass [
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	
	self deny: (p1 includesClass: a2).
	p1 addMethod: (a2>>#methodPackagedInP1).
	self deny: (p1 includesClass: a2).
	"We should declare the class explictly. Adding a method does not declare
	the class as defined. The reason is that like that the client controls the granularity 
	and moment of class registration."
	
	p1 addClassDefinition: a2.
	self assert: (p1 includesClass: a2).
	self assert: (p1 includesClassNamed: a2 name).
	
	
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludeClassMore [
	| p1 p2 p3 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	
	self assert: (p2 includesClass: a2).
	self deny: (p1 includesClass: a2).
	self deny: (p3 includesClass: a2).
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludeSelectorOfClass [
	| p1 p2 p3 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	
	"includesSelector checks both in defined and extension so we test both"
	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2 name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2 name).
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludeSelectorOfMetaClass [
	| p1 p2 p3 a2  a2class |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2class := a2 class.
	a2class compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2class>>#methodDefinedInP2).
	a2class compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2class>>#methodDefinedInP1).
	a2class compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2class>>#methodDefinedInP3).
	
	"includesSelector checks both in defined and extension so we test both"
	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a2class).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofMetaclassName: a2 name).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClass: a2class).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofMetaclassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofMetaclassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofMetaclassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP1 ofMetaclassName: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofMetaclassName: a2 name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClass: a2 class).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClass: a2).

]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludesMethodOfClassInPresenceOfOtherPackageExtensions [
	| p1 p2 p3 a2  a2name |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2name := #A2InPackageP2.
	a2 := self createNewClassNamed: a2name inPackage: p2.
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2name).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2name).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2name).
		
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2name).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2name).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2name).
	
	
	
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testIncludesOrTouches [
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	
	self assert: p1 extensionMethods size equals: 1.
	self assert: p1 extensionSelectors size equals: 1.
	self deny: (p1 includesClass: a2).
	self assert: (p1 definesOrExtendsClass: a2).
	self assert: (p2 includesClass: a2).
]

{ #category : #'test method addition removal' }
RPackageIncrementalTest >> testMethodAddition [
	| p1 a1 |
	p1 := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	a1 compileSilently: 'foo ^ 10'.
	p1 addMethod:  (a1>>#foo).
	self assert: (p1 includesSelector: #foo ofClass: a1).
	
]

{ #category : #'test method addition removal' }
RPackageIncrementalTest >> testMethodPackageResolution [
	| a1Name p1 a1 |
	a1Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: a1Name inPackage: p1.
	a1 compileSilently: 'method ^ #methodDefinedInP2'.
	a1 class compileSilently: 'method ^ #methodDefinedInP2'.
	
	p1 addMethod:  (a1>>#method).
	p1 addMethod:  (a1 class>>#method).
	
	self assert: (a1 >> #method) package == p1.
	self assert: (a1 class >> #method) package == p1.
]

{ #category : #'test package belonging' }
RPackageIncrementalTest >> testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt [
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	self assert: a2 package equals: p2.
	p1 extensionMethods do: 
		[:each | self deny: each methodClass package = p1 ].
	"the package of a class which is extended inside a package p, is not p
	but the package where the class was defined"
]

{ #category : #'test package belonging' }
RPackageIncrementalTest >> testPackageOfClassForDefinedClasses [
	| p a1 b1 |
	p := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1InPAckageP1 inPackage: p.
	b1 := self createNewClassNamed: #B1InPAckageP1 inPackage: p.

	self assert: a1 package equals: p.
	self assert: b1 package equals: p.

]

{ #category : #'test registration' }
RPackageIncrementalTest >> testPrivateClassRegisterUnregister [
	"Verify that when we register a class, the package organizer register it."
	| p a1 |
	p := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1InPackageP1.
	p definedClassNames add: #A1InPackageP1.
		"ugly but necessary to test registerClass: independantly from addClassDefinition:"
		"No event should be raised."
	p registerClass: a1.
	self assert: (self packageClass organizer packageOf: a1) equals: p.
	p definedClassNames remove: #A1InPackageP1.
	p unregisterClass: a1.
	self assert: (self packageClass organizer packageOf: a1) isDefault.
	
	p definedClassNames add: #A1InPackageP1.
		"ugly but necessary to test registerClass: independant from addClassDefinition:"
	p registerClassName: a1 name.
	self assert: (self packageClass organizer packageOf: a1) equals: p.
	p definedClassNames remove: #A1InPackageP1.
	p unregisterClassName: a1 name.
	self assert: (self packageClass organizer packageOf: a1) isDefault
	
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testRemoveClassAfterMethods [
	| p1 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size equals: 1.
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#newlyAddedToA1).
	
	"now we add a new method to the class and the to the package: it is considered
	as defined"
	self deny: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	p1 removeClassDefinition: a1.
	self assert: p1 definedClasses size equals: 0.
	
	self assert: (p1 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	"Now we add the class and this means that the method is not defined anymore but an extension "
	
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testRemoveClassRemovesExtensions [
	| p1 p2 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size equals: 1.
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p2 addMethod: (a1>>#newlyAddedToA1).
	
	self assert: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	
	a1 removeFromSystem.
	
	self deny: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesDefinedSelector: #newlyAddedToA1 ofClass: a1)
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testRemoveExtensionMethodRemovesExtensionsFromRPackage [
	| p1 p2 a1 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size equals: 1.
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p2 addMethod: (a1>>#newlyAddedToA1).
	
	self assert: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	
	a1 removeSelector: #newlyAddedToA1.
	
	self deny: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesDefinedSelector: #newlyAddedToA1 ofClass: a1)
]

{ #category : #'test extension' }
RPackageIncrementalTest >> testTwoClassesWithExtensions [	
	| p1 p2 a2  b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	b2 class compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (b2 class>>#methodPackagedInP1).
	
	self assert: p1 classes size equals: 2.
	self assert: p2 classes size equals: 2
]

{ #category : #'test class addition removal' }
RPackageIncrementalTest >> testUniqueClassInDefinedClassesUsingAddClassDefinition [
	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size equals: 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	p addClassDefinition: a1.
	self assert: p definedClasses size equals: 1.
	p addClassDefinition: a1 class.
	self assert: p definedClasses size equals: 1


]
