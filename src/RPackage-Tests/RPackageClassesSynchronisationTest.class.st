"
SUnit tests for RPackage classes synchronisation
"
Class {
	#name : 'RPackageClassesSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testAddExtensionProtocol [
	"test that by adding a protocol, nothing change from  Package point of vue."

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'newMethod' inClass: class asExtensionOf: xPackage.

	class addProtocol: '*yyyyy'.

	self assert: (self organizer hasPackage: #Yyyyy)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testAddNewProtocolDoesNothing [
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

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassMethodsInTheNewPackage [
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
RPackageClassesSynchronisationTest >> testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage [
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
RPackageClassesSynchronisationTest >> testRecategorizeClassWithMetaClassMethodsRegisterAllClassMethodsInTheNewPackage [
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

{ #category : 'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassDefinedMethodsFromItsPackage [
	"test that when we remove a class, the class methods defined in the parent package (not extensions) are all removed  from its parent Package"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassExtensionMethodsFromTheCorrespondingPackage [
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
RPackageClassesSynchronisationTest >> testRemoveProtocol [
	"test that by removing a protocol, the methods from this protocol are removed from the parent Package of the class"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	class removeProtocol: 'classic category'.

	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testRemovingExtensionProtocol [
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
RPackageClassesSynchronisationTest >> testRenameClass [

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #RPackageOldStubClass in: xPackage.

	class rename: #RPackageNewStubClass.

	self assert: (self organizer packageOf: class) equals: xPackage.
	self assert: (self organizer packageOfClassNamed: #RPackageNewStubClass) equals: xPackage.
	self assert: (self organizer packageOfClassNamed: #RPackageOldStubClass) equals: self organizer undefinedPackage.
	self assert: (xPackage includesClassNamed: 'RPackageNewStubClass').
	self deny: (xPackage includesClassNamed: 'RPackageOldStubClass')
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'classDefinedSelectors' dictionary of the parent package is updated with the new name. There fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'RPackageOldStubClass' asSymbol in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	class rename: 'RPackageNewStubClass'.

	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'classExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Therfore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #RPackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class rename: 'RPackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'metaclassDefinedSelectors' dictionary of the parent package is updated with the new name. Ther fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #RPackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class class inProtocol: 'classic category'.

	class rename: 'RPackageNewStubClass'.

	self assert: (class class >> #stubMethod) package equals: xPackage.
	self deny: (class class >> #stubMethod) isExtension
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'metaclassExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Ther fore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #RPackageOldStubClass in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class class asExtensionOf: yPackage.

	class rename: 'RPackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateOrganizerClassExtendingPackagesMapping [
	"test that when we rename a class, the classExtendingPackages dictionary of the organizer is updated with the new name, so that we can access the packages when specifying the new name"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: #RPackageOldStubClass in: xPackage.
	self createMethodNamed: #stubMethod inClass: class asExtensionOf: yPackage.

	class rename: 'RPackageNewStubClass'.

	self assert: (class extendingPackages includes: yPackage)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testRenamingExtensionProtocolToAnotherExtension [
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
RPackageClassesSynchronisationTest >> testRenamingExtenstionProtocolToClassicProtocol [
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
RPackageClassesSynchronisationTest >> testRenamingProtocolToMakeItAnExtension [
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
