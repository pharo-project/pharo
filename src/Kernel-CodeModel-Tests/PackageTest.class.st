"
SUnit tests for Package 
"
Class {
	#name : 'PackageTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'tests - classes' }
PackageTest >> testAddClass [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self assert: (xPackage includesClass: class).
	self assert: class package equals: xPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: xPackage.

	yPackage := self ensureYPackage.
	yPackage addClass: class.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - classes' }
PackageTest >> testAddClassSettingPackage [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self assert: (xPackage includesClass: class).
	self assert: class package equals: xPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: xPackage.

	yPackage := self ensureYPackage.
	class package: yPackage.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - classes' }
PackageTest >> testAddTrait [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newTraitNamed: #NewClass in: xPackage.

	self assert: (xPackage includesClass: class).
	self assert: class package equals: xPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: xPackage.

	yPackage := self ensureYPackage.
	yPackage addClass: class.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - classes' }
PackageTest >> testAddTraitSettingPackage [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newTraitNamed: #NewClass in: xPackage.

	self assert: (xPackage includesClass: class).
	self assert: class package equals: xPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: xPackage.

	yPackage := self ensureYPackage.
	class package: yPackage.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests' }
PackageTest >> testAllUnsentMessages [

	| package class1 class2 |
	package := self ensurePackage: #Test1.

	class1 := self newClassNamed: 'TestClass' in: package tag: 'TAG'.
	class2 := self newClassNamed: 'TestClassOther' in: package tag: 'TAG'.

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
PackageTest >> testAnonymousClassAndSelector [
	"jgeidel test case from issue 12628."

	"Make sure we don't have a registration or a package for the method."

	| ghost method |
	ghost := Object newAnonymousSubclassInEnvironment: testEnvironment.
	method := ghost compiler compile: 'rpackagetest'.
	ghost addSelector: #packagetest withMethod: method.
	self deny: (self organizer undefinedPackage includesSelector: #packagetest ofClass: ghost).
	self assert: (self organizer packageOfClassNamed: ghost name) equals: self organizer undefinedPackage.
	ghost classify: #packagetest under: '*packagetest'
]

{ #category : 'tests - extensions' }
PackageTest >> testAsPackage [
	self assert: self class package name asPackage isNotNil
]

{ #category : 'tests - extensions' }
PackageTest >> testAsPackageIfAbsent [
	self assert: ('___inexistent___package___' asPackageIfAbsent: [ true ])
]

{ #category : 'tests - extensions' }
PackageTest >> testAsPackageWithError [

	self should: [ '___inexistent___package___' asPackage ] raise: NotFound
]

{ #category : 'tests - demotion' }
PackageTest >> testDemoteToPackageNamed [

	| package1 package2 class |
	package1 := self ensurePackage: #'Test1-TAG1'.
	class := self newClassNamed: 'TestClass' in: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1'.
	self assert: package2 isNotNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 tagNamed: 'TAG1') classes includes: class)
]

{ #category : 'tests - demotion' }
PackageTest >> testDemoteToPackageNamedExistingPackage [

	| package1 package2 packageExisting class |
	package1 := self ensurePackage: #'Test1-TAG1'.
	packageExisting := self ensurePackage: #Test1.
	class := self newClassNamed: 'TestClass' in: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1'.
	self assert: package2 isNotNil.
	self assert: package2 equals: packageExisting.
	self assert: (package2 classes includes: class).
	self assert: ((package2 tagNamed: 'TAG1') classes includes: class)
]

{ #category : 'tests - demotion' }
PackageTest >> testDemoteToPackageNamedKeepOrganizer [

	| newOrganizer package renamedPackage |
	newOrganizer := PackageOrganizer new.

	package := newOrganizer ensurePackage: #'Test1-TAG1'.

	renamedPackage := package demoteToTagInPackage.

	self assert: renamedPackage organizer identicalTo: newOrganizer
]

{ #category : 'tests - demotion' }
PackageTest >> testDemoteToPackageNamedMultilevelPackage [

	| package1 package2 class |
	package1 := self ensurePackage: #'Test1-TAG1-X1'.
	class := self newClassNamed: 'TestClass' in: package1.
	class compile: 'foo ^42' classified: 'accessing'.

	package1 demoteToTagInPackage.

	self deny: (self organizer hasPackage: package1).
	package2 := self organizer packageNamed: 'Test1-TAG1'.
	self assert: package2 isNotNil.
	self assert: (package2 classes includes: class).
	self assert: ((package2 tagNamed: 'X1') classes includes: class)
]

{ #category : 'tests - demotion' }
PackageTest >> testDemoteToPackageNamedWithExtension [

	| packageOriginal packageDemoted class classOther |
	packageOriginal := self ensurePackage: #'Test1-TAG1'.
	class := self newClassNamed: 'TestClass' in: packageOriginal.
	class compile: 'foo ^42' classified: 'accessing'.

	classOther := self newClassNamed: 'TestClassOther' in: 'XXXX'.
	classOther compile: 'bar ^42' classified: #'*Test1-TAG1'.

	packageOriginal demoteToTagInPackage.

	self deny: (self organizer hasPackage: packageOriginal).
	packageDemoted := self organizer packageNamed: 'Test1'.
	self assert: packageDemoted isNotNil.
	self assert: (packageDemoted classes includes: class).
	self assert: ((packageDemoted tagNamed: 'TAG1') classes includes: class).
	self assert: (packageDemoted extensionMethods includes: classOther >> #bar).
	self assert: (classOther >> #bar) protocolName equals: '*Test1-TAG1'.
	self assert: (packageDemoted classes includesAll: {
				 class.
				 classOther })
]

{ #category : 'tests - properties' }
PackageTest >> testHasProperty [
	| package testValue |

	testValue := Date today.
	package := self class package.

	package propertyAt: #testKeySelector put: testValue.
	self	assert: (package hasProperty: #testKeySelector).

	package removeProperty: #testKeySelector.
	self deny: (package hasProperty: #testKeySelector)
]

{ #category : 'tests - queries' }
PackageTest >> testHierarchyRoots [

	| roots |
	roots := (self packageOrganizer packageNamed: #'Kernel-CodeModel-Tests') hierarchyRoots.
	roots := roots collect: [ :each | each name ].
	#( #PackageTestCase ) do: [ :each | roots includes: each ]
]

{ #category : 'tests' }
PackageTest >> testIncludeClassWithExtensions [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (yPackage includesClass: a2).
	self deny: (xPackage includesClass: a2).
	self deny: (zPackage includesClass: a2)
]

{ #category : 'tests' }
PackageTest >> testIncludeSelectorOfClass [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2).

	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).

	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : 'tests' }
PackageTest >> testIncludeSelectorOfMetaClass [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 class compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 class compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 class compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2 class).
	self assert: (a2 class >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 class >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2 class).

	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2 class).

	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2 class).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2 class)
]

{ #category : 'tests' }
PackageTest >> testIncludesMethodOfClassInPresenceOfOtherPackageExtensions [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : 'tests - extensions' }
PackageTest >> testIsPackage [

	self assert: self class package name asPackage isPackage
]

{ #category : 'tests' }
PackageTest >> testIsTestPackage [

	| package |
	package := self organizer ensurePackage: 'MockPackage-Tests'.
	self assert: package isTestPackage. "Happy case: test package 'MockPackage-Tests' must contain -Tests suffix."

	package := self organizer ensurePackage: 'MockPackage2-tests'.
	self assert: package isTestPackage. "Package 'MockPackage-tests' is not test package, since it has lowercase suffix."

	package := self organizer ensurePackage: 'MockPackage'.
	self deny: package isTestPackage. "Happy case: regular package 'MockPackage' without -Tests suffix is not a test package."

	package := self organizer ensurePackage: 'MockPackage-Tests-Package'.
	self assert: package isTestPackage "Package 'MockPackage-Tests-Package' containting -Tests- in middle, so it is test package."
]

{ #category : 'tests - MC' }
PackageTest >> testMcPackage [

	| rPackage |
	rPackage := self organizer ensurePackage: #Test1.
	self assert: rPackage mcPackage equals: (MCPackage new name: #Test1)
]

{ #category : 'tests - classes' }
PackageTest >> testMoveClassToTag [

	| xPackage yPackage yTag class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage tag: #TAG.

	self assert: (xPackage includesClass: class).
	self assert: (xPackage hasTag: #TAG).
	self assert: ((xPackage tagNamed: #TAG) includesClass: class).

	yPackage := self ensureYPackage.
	yTag := yPackage ensureTag: #YTag.
	yPackage moveClass: class toTag: yTag.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: (yTag includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag equals: yTag.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - classes' }
PackageTest >> testMoveClassToTagInDefaultTag [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self assert: (xPackage includesClass: class).
	self assert: class package equals: xPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: xPackage.

	yPackage := self ensureYPackage.
	yPackage moveClass: class toTag: yPackage rootTag.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag isRoot.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - classes' }
PackageTest >> testMoveClassToTagName [

	| xPackage yPackage yTag class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage tag: #TAG.

	self assert: (xPackage includesClass: class).
	self assert: (xPackage hasTag: #TAG).
	self assert: ((xPackage tagNamed: #TAG) includesClass: class).

	yPackage := self ensureYPackage.
	yPackage moveClass: class toTag: #YTag.
	yTag := yPackage tagNamed: #YTag.

	self deny: (xPackage includesClass: class).
	self assert: (yPackage includesClass: class).
	self assert: (yTag includesClass: class).
	self assert: class package equals: yPackage.
	self assert: class packageTag equals: yTag.
	self assert: class packageTag package equals: yPackage
]

{ #category : 'tests - properties' }
PackageTest >> testPropertyAtPut [

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
PackageTest >> testRemoveClassRemovesExtensions [

	| xPackage yPackage a1 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	"the class is created but not added to the package for now"
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , yPackage name.

	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeFromSystem.

	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : 'tests - classes' }
PackageTest >> testRemoveClassUsingEnvironment [

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self organizer environment removeClassNamed: #NewClass.
	self deny: (xPackage includesClass: class)
]

{ #category : 'tests' }
PackageTest >> testRemoveEmptyTags [

	| package class tag1 tag2 |
	package := self ensurePackage: #Test1.

	tag1 := package ensureTag: #Tag1.
	tag2 := package ensureTag: #Tag2.

	class := self newClassNamed: 'TestClass' inTag: tag1.

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
PackageTest >> testRemoveFromSystemRemoveAllTags [
	"Regression test because #removeFromSystem was removing the tags while iterating on them and that caused troubles."

	| xPackage tag1 tag2 tag3 |
	xPackage := self ensureXPackage.
	tag1 := xPackage ensureTag: #Tag1.
	tag2 := xPackage ensureTag: #Tag2.
	tag3 := xPackage ensureTag: #Tag3.

	self assert: (xPackage hasTag: tag1).
	self assert: (xPackage hasTag: tag2).
	self assert: (xPackage hasTag: tag3).

	xPackage removeFromSystem.

	self deny: (xPackage hasTag: tag1).
	self deny: (xPackage hasTag: tag2).
	self deny: (xPackage hasTag: tag3)
]

{ #category : 'tests' }
PackageTest >> testRemoveTag [

	| p1 a1 b1 |
	p1 := self ensurePackage: #P1.

	a1 := self newClassNamed: #A1DefinedInX in: p1.
	b1 := self newClassNamed: #B1DefinedInX in: p1.
	self assert: p1 tags size equals: 1. "We start with the root tag"

	p1 moveClass: a1 toTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.

	p1 moveClass: b1 toTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 removeTag: #bar.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 removeTag: #foo.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assertEmpty: (p1 classesTaggedWith: #foo)
]

{ #category : 'tests' }
PackageTest >> testRemoveTagRemoveClasses [

	| p1 a1 |
	p1 := self ensurePackage: #P1.

	a1 := self newClassNamed: #A1DefinedInX in: p1.
	self assert: p1 tags size equals: 1. "We start with the root tag"

	p1 moveClass: a1 toTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.

	p1 removeTag: #foo.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assertEmpty: (p1 classesTaggedWith: #foo).
	self deny: (p1 includesClass: a1).
	self assert: (self organizer packageOf: a1) name equals: UndefinedPackage undefinedPackageName.
	self assert: a1 isObsolete
]

{ #category : 'tests' }
PackageTest >> testRenamePackageAlsoRenameAllExtensionProtocols [
	"test that when we rename a category, all corresponding extension protocols in the system are renamed"

	| p1 p2 p3 classInY classInZ |
	p1 := self ensurePackage: #Test1.
	p2 := self ensurePackage: #Test2.
	p3 := self ensurePackage: #Test3.

	classInY := self newClassNamed: 'ClassInYPackage' in: p2.
	classInZ := self newClassNamed: 'ClassInZPackage' in: p3.

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
PackageTest >> testRenameUpdateTheOrganizer [
	"test that when we rename a category, the organizer dictionary is update with this new name, so that we can access the package with this new name as key"

	| package |
	package := self ensurePackage: #Test1.

	package renameTo: #Test2.
	self assert: package name equals: #Test2.
	self assert: (self organizer packageNamed: #Test2) identicalTo: package.
	self deny: (self organizer hasPackage: #Test1)
]

{ #category : 'tests' }
PackageTest >> testTwoClassesWithExtensions [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.
	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage classes size equals: 2.
	self assert: yPackage classes size equals: 2
]

{ #category : 'tests' }
PackageTest >> testUniqueClassInDefinedClassesUsingAddClass [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: a1 class.
	self assert: xPackage definedClasses size equals: 1
]
