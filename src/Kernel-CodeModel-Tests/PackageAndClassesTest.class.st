"
SUnit tests for RPackage classes synchronisation
"
Class {
	#name : 'PackageAndClassesTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'tests' }
PackageAndClassesTest >> testAddClassNoDuplicate [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	b1 := self newClassNamed: #B1InPackageP1 in: self yPackageName.
	xPackage addClass: a1.
	"adding the same class does not do anything - luckily"
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: b1.
	self assert: xPackage definedClasses size equals: 2
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testAddExtensionProtocol [
	"test that by adding a protocol, nothing change from  Package point of vue."

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'newMethod' inClass: class asExtensionOf: xPackage.

	class addProtocol: '*yyyyy'.

	self assert: (self organizer hasPackage: #Yyyyy)
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testAddNewProtocolDoesNothing [
	"test that by adding a protocol, nothing change from Package point of vue."

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'newMethod' inClass: class asExtensionOf: xPackage.

	class addProtocol: 'accessing'.

	self assert: (xPackage includesClass: class).
	self assert: (class >> #newMethod) package equals: xPackage.
	self deny: (class >> #newMethod) isExtension.
	self assertEmpty: xPackage extensionMethods
]

{ #category : 'tests' }
PackageAndClassesTest >> testClassAddition [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: a1 class)
]

{ #category : 'tests' }
PackageAndClassesTest >> testDefinedClassesAndDefinedClassNames [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	self assert: (xPackage definedClasses includes: a1).
	self assert: (xPackage definedClassNames includes: a1 name).

	b1 := self newClassNamed: #B1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 2.
	self assert: (xPackage definedClasses includes: b1).
	self assert: (xPackage definedClassNames includes: b1 name)
]

{ #category : 'tests' }
PackageAndClassesTest >> testExtensionClassNames [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: b2).
	self assert: (yPackage includesClass: a2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 1.
	self assert: xPackage extensionMethods size equals: 1.
	self assert: (xPackage extendedClassNames includes: #A2InPackageP2).
	self deny: (xPackage includesClass: a2). "method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 2.
	self assert: xPackage extensionMethods size equals: 2.
	self assert: (xPackage extendedClassNames includes: #B2InPackageP2).
	self deny: (xPackage includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 3.
	self assert: xPackage extensionMethods size equals: 3.
	self assert: xPackage extendedClasses size equals: 2.
	self assert: (xPackage extendedClassNames includes: #B2InPackageP2).
	self deny: (xPackage includesClass: b2)
]

{ #category : 'tests' }
PackageAndClassesTest >> testExtensionClasses [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: a2).

	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage extendedClasses size equals: 1.
	self assert: (xPackage extendedClasses includes: a2).
	self assert: xPackage extendedClassNames size equals: 1.
	self assert: (xPackage extendedClassNames includes: a2 name).

	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage extendedClasses size equals: 2.
	self assert: (xPackage extendedClasses includes: b2).
	"extensionClasses returns or metaclasses while extensionClassNames returns class names (but not metaclass names)"

	self assert: xPackage extendedClassNames size equals: 2.
	self assert: (xPackage extendedClassNames includes: b2 name)
]

{ #category : 'tests' }
PackageAndClassesTest >> testExtensionClassesWithCompiledMethod [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: b2).
	self assert: (yPackage includesClass: b2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionMethods size equals: 1.
	self assert: xPackage extensionSelectors size equals: 1.
	self assert: xPackage extendedClasses size equals: 1.
	self deny: (xPackage includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 2.
	self assert: xPackage extensionMethods size equals: 2.
	self assert: xPackage extendedClasses size equals: 2.
	self deny: (xPackage includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 3.
	self assert: xPackage extensionMethods size equals: 3.
	self assert: xPackage extendedClasses size equals: 2.
	self deny: (xPackage includesClass: b2)
]

{ #category : 'tests' }
PackageAndClassesTest >> testIncludeClass [

	| xPackage yPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self deny: (xPackage includesClass: a2).
	xPackage addMethod: a2 >> #methodPackagedInP1.
	self deny: (xPackage includesClass: a2).
	"We should declare the class explictly. Adding a method does not declare
	the class as defined. The reason is that like that the client controls the granularity
	and moment of class registration."

	xPackage addClass: a2.
	self assert: (xPackage includesClass: a2).
	self assert: (xPackage includesClassNamed: a2 name)
]

{ #category : 'tests' }
PackageAndClassesTest >> testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt [

	| xPackage yPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: a2 package equals: yPackage.
	xPackage extensionMethods do: [ :each | "the package of a class which is extended inside a package p, is not p
	but the package where the class was defined"
		self deny: each methodClass package equals: xPackage ]
]

{ #category : 'tests' }
PackageAndClassesTest >> testPackageOfClassForDefinedClasses [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: xPackage.
	b1 := self newClassNamed: #B1InPAckageP1 in: xPackage.

	self assert: a1 package equals: xPackage.
	self assert: b1 package equals: xPackage
]

{ #category : 'tests - recategorizing class' }
PackageAndClassesTest >> testRecategorizeClassRegisterTheClassMethodsInTheNewPackage [
	"test that when we recategorize a class, the new package in which it is defined include all the methods defined in this class (not extensions)"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'method1' inClass: class inProtocol: 'category'.
	self createMethodNamed: 'method2' inClass: class asExtensionOf: zPackage.
	self createMethodNamed: 'method3' inClass: class asExtensionOf: yPackage.

	class package: yPackage.

	self assert: (class >> #method1) package equals: yPackage.
	self deny: (class >> #method1) isExtension.
	self deny: (xPackage includesSelector: #method1 ofClass: class).
	self deny: (zPackage includesSelector: #method1 ofClass: class).

	self deny: (xPackage includesSelector: #method2 ofClass: class).
	self deny: (yPackage includesSelector: #method2 ofClass: class).
	self assert: (zPackage includesExtensionSelector: #method2 ofClass: class).

	self deny: (xPackage includesSelector: #method3 ofClass: class).
	self assert: (class >> #method3) package equals: yPackage.
	self deny: (class >> #method3) isExtension.
	self deny: (zPackage includesSelector: #method3 ofClass: class)
]

{ #category : 'tests - recategorizing class' }
PackageAndClassesTest >> testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage [
	"test that when we recategorize a class, the old package in which it was defined don't include its defined methods (not extensions) anymore"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'method1' inClass: class inProtocol: 'category'.
	self createMethodNamed: 'method2' inClass: class asExtensionOf: zPackage.
	self createMethodNamed: 'method3' inClass: class asExtensionOf: yPackage.

	class package: yPackage.
	self deny: (xPackage includesSelector: #method1 ofClass: class).
	self deny: (xPackage includesSelector: #method2 ofClass: class).
	self deny: (xPackage includesSelector: #method3 ofClass: class)
]

{ #category : 'tests - recategorizing class' }
PackageAndClassesTest >> testRecategorizeClassWithMetaClassMethodsRegisterAllClassMethodsInTheNewPackage [
	"test that when we recategorize a class (having methods defined in both instance and class side), the new package in which it is defined include all the methods (from instance and class side) defined in this class (not extensions)"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'method1' inClass: class class inProtocol: 'category'.
	self createMethodNamed: 'method2' inClass: class class asExtensionOf: yPackage.
	self createMethodNamed: 'method3' inClass: class class asExtensionOf: zPackage.

	class package: yPackage.

	"lets check metaclass methods"
	self assert: (class class >> #method1) package equals: yPackage.
	self deny: (class class >> #method1) isExtension.
	self assert: (class class >> #method2) package equals: yPackage.
	self deny: (class class >> #method2) isExtension.
	self assert: (zPackage includesExtensionSelector: #method3 ofClass: class class)
]

{ #category : 'tests' }
PackageAndClassesTest >> testRemoveClass [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: self yPackageName.
	b1 := self newClassNamed: #B1InPAckageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.

	xPackage addClass: a1.
	xPackage addClass: b1.
	self assert: xPackage definedClasses size equals: 2.

	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self deny: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: b1.
	self deny: (xPackage includesClass: b1)
]

{ #category : 'tests' }
PackageAndClassesTest >> testRemoveClassInTag [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.

	a1 := self newClassNamed: #A1InPAckageP1 in: xPackage.
	b1 := self newClassNamed: #B1InPAckageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 2.

	xPackage moveClass: a1 toTag: 'a1-tag'.
	xPackage moveClass: b1 toTag: 'b1-tag'.
	self assert: xPackage tags size equals: 2.

	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self deny: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: b1.
	self deny: (xPackage includesClass: b1).
	self assert: xPackage tags size equals: 0
]

{ #category : 'tests - removing classes' }
PackageAndClassesTest >> testRemoveClassUnregisterTheClassDefinedMethodsFromItsPackage [
	"test that when we remove a class, the class methods defined in the parent package (not extensions) are all removed  from its parent Package"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - removing classes' }
PackageAndClassesTest >> testRemoveClassUnregisterTheClassExtensionMethodsFromTheCorrespondingPackage [
	"test that when we remove a class, the class extension methods are all removed  from theire corresponding parent Package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (yPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testRemoveProtocol [
	"test that by removing a protocol, the methods from this protocol are removed from the parent Package of the class"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	class removeProtocol: 'classic category'.

	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testRemovingExtensionProtocol [
	"test that by removing an extension protocol (a category protocol with '*'), all the methods that were inside are removed from the extending package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class removeProtocol: '*YYYYY'.

	self deny: (yPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClass [

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #PackageOldStubClass in: xPackage.

	class rename: #PackageNewStubClass.

	self assert: (self organizer packageOf: class) equals: xPackage.
	self assert: (self organizer packageOfClassNamed: #PackageNewStubClass) equals: xPackage.
	self assert: (self organizer packageOfClassNamed: #PackageOldStubClass) equals: self organizer undefinedPackage.
	self assert: (xPackage includesClassNamed: 'PackageNewStubClass').
	self deny: (xPackage includesClassNamed: 'PackageOldStubClass')
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClassUpdateClassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'classDefinedSelectors' dictionary of the parent package is updated with the new name. There fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'PackageOldStubClass' asSymbol in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic protocol'.

	class rename: 'PackageNewStubClass'.

	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClassUpdateClassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'classExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Therfore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #PackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class rename: 'PackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClassUpdateMetaclassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'metaclassDefinedSelectors' dictionary of the parent package is updated with the new name. Ther fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #PackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class class inProtocol: 'classic protocol'.

	class rename: 'PackageNewStubClass'.

	self assert: (class class >> #stubMethod) package equals: xPackage.
	self deny: (class class >> #stubMethod) isExtension
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClassUpdateMetaclassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'metaclassExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Ther fore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #PackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class class asExtensionOf: yPackage.

	class rename: 'PackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class class)
]

{ #category : 'tests - operations on classes' }
PackageAndClassesTest >> testRenameClassUpdateOrganizerClassExtendingPackagesMapping [
	"test that when we rename a class, the classExtendingPackages dictionary of the organizer is updated with the new name, so that we can access the packages when specifying the new name"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #PackageOldStubClass in: xPackage.
	self createMethodNamed: #stubMethod inClass: class asExtensionOf: yPackage.

	class rename: 'PackageNewStubClass'.

	self assert: (class extendingPackages includes: yPackage)
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testRenamingExtensionProtocolToAnotherExtension [
	"test that by renaming an extension protocol (a protocol beginning with '*') to another extension protocol, all the methods are moved to the new extendingPackage"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class renameProtocol: '*YYYYY' as: '*zzzzz'.

	self assert: (zPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testRenamingExtenstionProtocolToClassicProtocol [
	"test that by renaming an extension protocol (a protocol beginning with '*') to a classic protocol, all the methods are moved from the extendingPackage to the parent package of the class"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.
	self createMethodNamed: 'stubMethod2' inClass: class asExtensionOf: yPackage.

	class renameProtocol: '*YYYYY' as: 'classic category'.

	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self assert: (class >> #stubMethod2) package equals: xPackage.
	self deny: (class >> #stubMethod2) isExtension
]

{ #category : 'tests - operations on protocols' }
PackageAndClassesTest >> testRenamingProtocolToMakeItAnExtension [
	"test that by renaming a  classic protocol (a protocol not beginning with '*') to an extension protocol, all the methods are moved from the parent package of the class to the extending package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.
	self createMethodNamed: 'stubMethod2' inClass: class inProtocol: 'classic category'.

	class renameProtocol: 'classic category' as: '*yyyyy'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesSelector: #stubMethod2 ofClass: class)
]
