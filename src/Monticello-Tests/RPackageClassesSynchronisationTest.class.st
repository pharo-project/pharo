"
SUnit tests for RPackage classes synchronisation
"
Class {
	#name : 'RPackageClassesSynchronisationTest',
	#superclass : 'RPackageMCSynchronisationTest',
	#category : 'Monticello-Tests-RPackage',
	#package : 'Monticello-Tests',
	#tag : 'RPackage'
}

{ #category : 'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassAddItIntoPackageBestMatchName [

	| package tag class |
	tag := self ensureTagYinX.
	package := tag package.

	class := self newClassNamed: 'NewClass' inTag: tag.

	self assert: (package definesClass: class).
	self assert: class package equals: package
]

{ #category : 'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassAddItIntoPackageExactName [

	| package tag class |
	tag := self ensureTagYinX.
	package := tag package.

	class := self newClassNamed: 'NewClass' inTag: tag.

	self assert: (package definesClass: class).
	self assert: class package equals: package
]

{ #category : 'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassUpdateTheOrganizerMappings [
	"test that when we add a class, the organizer 'classPackageMapping' dictionary is updated, so that the class is linked to its package and so that we can access its owning package"

	| package class |
	package := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: package.

	self assert: class package equals: package
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRaisesClassRepackagedEvent [
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"

	| xPackage yPackage class ann |
	SystemAnnouncer uniqueInstance when: ClassRepackaged do: [ :a | ann := a ] for: self.

	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	class category: 'YYYYY'.

	self assert: ann isNotNil.
	self assert: ann classRepackaged equals: class.
	self assert: ann oldPackage equals: xPackage.
	self assert: ann newPackage equals: yPackage
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheBestMatchPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	self ensureTagYinX.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: yPackage.

	class category: 'XXXXX-YYYY'.
	self assert: (self organizer packageOf: class) equals: xPackage.
	self deny: (self organizer packageOf: class) equals: yPackage
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheExactMatchPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	self ensureTagYinX.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: yPackage.

	class category: 'XXXXX'.

	self assert: (self organizer packageOf: class) equals: xPackage.
	self deny: (self organizer packageOf: class) equals: yPackage
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheMatchingPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: yPackage.

	class category: 'XXXXX'.
	self deny: (self organizer packageOf: class) equals: yPackage.
	self assert: (self organizer packageOf: class) equals: xPackage
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassMethodsInTheNewPackage [
	"test that when we recategorize a class, the new package in which it is defined include all the methods defined in this class (not extensions)"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'method1' inClass: class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class inCategory: '*zzzzz'.
	self createMethodNamed: 'method3' inClass: class inCategory: '*yyyyy'.

	class category: 'YYYYY'.

	self assert: (yPackage includesDefinedSelector: #method1 ofClass: class).
	self deny: (xPackage includesSelector: #method1 ofClass: class).
	self deny: (zPackage includesSelector: #method1 ofClass: class).

	self deny: (xPackage includesSelector: #method2 ofClass: class).
	self deny: (yPackage includesSelector: #method2 ofClass: class).
	self assert: (zPackage includesExtensionSelector: #method2 ofClass: class).

	self deny: (xPackage includesSelector: #method3 ofClass: class).
	self assert: (yPackage includesDefinedSelector: #method3 ofClass: class).
	self deny: (zPackage includesSelector: #method3 ofClass: class)
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUnregisterTheClassFromTheOldPackage [
	"test that when we recategorize a class, the old package in which it was defined don't include it anymore"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	class category: 'YYYYY'.
	self deny: (xPackage includesClass: class)
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage [
	"test that when we recategorize a class, the old package in which it was defined don't include its defined methods (not extensions) anymore"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'method1' inClass: class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class inCategory: '*zzzzz'.
	self createMethodNamed: 'method3' inClass: class inCategory: '*yyyyy'.

	class category: 'YYYYY'.
	self deny: (xPackage includesSelector: #method1 ofClass: class).
	self deny: (xPackage includesSelector: #method2 ofClass: class).
	self deny: (xPackage includesSelector: #method3 ofClass: class)
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUpdateTheOrganizerMappings [
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"

	| yPackage class |
	self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: 'XXXXX'.
	class category: 'YYYYY'.
	self assert: (self organizer packageOf: class) equals: yPackage
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassWithMetaClassMethodsRegisterAllClassMethodsInTheNewPackage [
	"test that when we recategorize a class (having methods defined in both instance and class side), the new package in which it is defined include all the methods (from instance and class side) defined in this class (not extensions)"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'method1' inClass: class class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class class inCategory: '*yyyyy'.
	self createMethodNamed: 'method3' inClass: class class inCategory: '*zzzzz'.

	class category: 'YYYYY'.

	"lets check metaclass methods"
	self assert: (yPackage includesDefinedSelector: #method1 ofClass: class class).
	self assert: (yPackage includesDefinedSelector: #method2 ofClass: class class).
	self assert: (zPackage includesExtensionSelector: #method3 ofClass: class class)
]

{ #category : 'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassWithUnexistingPackageNameRegisterTheClassInANewPackage [
	"test that when we recategorize a class in category not yet registered in RPackage, a new rPackage is created with the class inside"

	| xPackage yYPackage class |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self assert: (self organizer packageOf: class) equals: xPackage.

	class category: 'YYYYY'.
	yYPackage := self organizer packageNamed: #YYYYY.

	self assert: (self organizer packageOf: class) equals: yYPackage.
	self deny: (self organizer packageOf: class) equals: xPackage
]

{ #category : 'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassDefinedMethodsFromItsPackage [
	"test that when we remove a class, the class methods defined in the parent package (not extensions) are all removed  from its parent Package"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

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
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (yPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassFromItsPackage [
	"test that when we remove a class, the class is removed from its parent Package"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (xPackage includesClass: class)
]

{ #category : 'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUpdateTheOrganizerMappings [
	"test that when we remove a class, the organizer is updated so that the class is no longer present in the  'classPackageDictionary' dictionary"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self organizer environment removeClassNamed: 'NewClass'.
	self deny: (self organizer packageOf: class) isNil
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassDefinedInTheParentPackage [
	"test that when we rename a class, the classes Defined in the parent package includes the new name"

	| xPackage class |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	class rename: 'RPackageNewStubClass'.

	self assert: (xPackage includesClassNamed: 'RPackageNewStubClass').
	self deny: (xPackage includesClassNamed: 'RPackageOldStubClass')
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'classDefinedSelectors' dictionary of the parent package is updated with the new name. There fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class rename: 'RPackageNewStubClass'.

	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'classExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Therfore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class rename: 'RPackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'metaclassDefinedSelectors' dictionary of the parent package is updated with the new name. Ther fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class inCategory: 'classic category'.

	class rename: 'RPackageNewStubClass'.

	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'metaclassExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Ther fore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class inCategory: '*yyyyy'.

	class rename: 'RPackageNewStubClass'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class class)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateOrganizerClassExtendingPackagesMapping [
	"test that when we rename a class, the classExtendingPackages dictionary of the organizer is updated with the new name, so that we can access the packages when specifying the new name"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.
	self createMethodNamed: #stubMethod inClass: class inCategory: '*yyyyy'.

	class rename: 'RPackageNewStubClass'.

	self assert: ((self organizer extendingPackagesOf: class) includes: yPackage)
]

{ #category : 'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateOrganizerClassPackageMapping [
	"test that when we rename a class, the classPackage dictionary of the organizer is updated with the new name, so that we can access the package when specifying the new name"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'RPackageOldStubClass' in: xPackage.

	class rename: 'RPackageNewStubClass'.

	self assert: (self organizer packageOf: class) equals: xPackage.
	self assert: (self organizer packageOfClassNamed: 'RPackageOldStubClass' asSymbol) equals: nil
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByAddingExtensionProtocol [
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."

	| xPackage class |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.
	class addProtocol: '*yyyyy'.

	self assert: (self organizer hasPackage: #Yyyyy)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByAddingNewProtocolDoesNothing [
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.
	class addProtocol: 'accessing'.

	self assert: (xPackage includesClass: class).
	self assert: (xPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: xPackage extensionMethods size equals: 0
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRemovingClassicCategoryRemoveMethodsFromTheParentPackage [
	"test that when we reoganized a class by removing a classic category (a category not beginning with '*'), the methods from this category are removed from the parent RPackage of the class"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.
	class removeProtocol: 'classic category'.
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRemovingExtensionCategoryRemoveMethodsFromTheExtendingPackage [
	"test that when we reoganized a class by removing an extension category (a category beginning with '*'), all the methods that were inside are removed from the extending package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class removeProtocol: '*yyyyy'.

	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingClassicCategoryToExtensionCategoryMoveAllMethodsFromParentPackageToExtendingPackage [
	"test that when we reoganized a class by renaming a  classic category (a category not beginning with '*') to an extension category, all the methods are moved from the  parent package of the class to the extending package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.
	self createMethodNamed: 'stubMethod2' inClass: class inCategory: 'classic category'.

	class renameProtocol: 'classic category' as: '*yyyyy'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod2 ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingClassicCategoryToExtensionCategoryMoveMethodsFromParentPackageToExtendingPackage [
	"test that when we reoganized a class by renaming a  classic category (a category not beginning with '*') to an extension category, all the methods are moved from the  parent package of the class to the extending package"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.
	class renameProtocol: 'classic category' as: '*yyyyy'.
	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToAnotherExtensionCategoryAddMethodsToTheNewPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to another extension category, all the methods are moved to the new extendingPackage"

	| xPackage yPackage zPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	class renameProtocol: '*yyyyy' as: '*zzzzz'.
	self assert: (zPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveAllMethodsFromExtendingPackageToParentPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class inCategory: '*yyyyy'.

	class renameProtocol: '*yyyyy' as: 'classic category'.

	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (xPackage includesDefinedSelector: #stubMethod2 ofClass: class)
]

{ #category : 'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveMethodsFromExtendingPackageToParentPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	class renameProtocol: '*yyyyy' as: 'classic category'.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]
