"
SUnit tests for RPackage classes synchronisation
"
Class {
	#name : #RPackageClassesSynchronisationTest,
	#superclass : #RPackageMCSynchronisationTest,
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassAddItIntoPackageBestMatchName [

	|tmpPackage class|
	self addXCategory.
	self addXMatchCategory.
	
	tmpPackage := self organizer packageNamed: #'XXXXX'.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX-YYYY'.

	self assert: (tmpPackage definesClass: class).
	self assert: class package equals: tmpPackage
]

{ #category : #'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassAddItIntoPackageExactName [

	|tmpPackage class|
	self addXCategory.
	self addXMatchCategory.

	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self assert: (tmpPackage definesClass: class).
	self assert: class package equals: tmpPackage
]

{ #category : #'tests - adding classes' }
RPackageClassesSynchronisationTest >> testAddClassUpdateTheOrganizerMappings [
	"test that when we add a class, the organizer 'classPackageMapping' dictionary is updated, so that the class is linked to its package and so that we can access its owning package"

	|tmpPackage class|
	self addXCategory.
	
	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	 
	self assert: class package equals: tmpPackage
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRaisesClassRepackagedEvent [
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"
	
	| XPackage YPackage class ann |
	
	ann := nil.
	SystemAnnouncer uniqueInstance
		when: ClassRepackaged do: [ :a |
			ann := a.
		].
	
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	class category: 'YYYYY'.	
	
	self assert: ann notNil.
	self assert: ann classRepackaged equals: class.
	self assert: ann oldPackage equals: XPackage.
	self assert: ann newPackage equals: YPackage.
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheBestMatchPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| XPackage YPackage class |
	self addXCategory.
	self addXMatchCategory.
	self addYCategory.

	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.

	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.

	class category: 'XXXXX-YYYY'.
	self assert: (self organizer packageOf: class) equals: XPackage.
	self deny: (self organizer packageOf: class) equals: YPackage
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheExactMatchPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| XPackage YPackage class |
	self addXCategory.
	self addXMatchCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.

	class category: 'XXXXX'.

	self assert: (self organizer packageOf: class) equals: XPackage.
	self deny: (self organizer packageOf: class) equals: YPackage
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassInTheMatchingPackage [
	"test that when we recategorize a class, the new package in which it is defined include it"

	| XPackage YPackage class |
	self addXCategory.
	self addYCategory.

	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.

	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.

	class category: 'XXXXX'.
	self deny: (self organizer packageOf: class) equals: YPackage.
	self assert: (self organizer packageOf: class) equals: XPackage
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassRegisterTheClassMethodsInTheNewPackage [

	"test that when we recategorize a class, the new package in which it is defined include all the methods defined in this class (not extensions)"
	
	| XPackage YPackage ZPackage class|
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'method1' inClass: class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class inCategory: '*zzzzz'.
	self createMethodNamed: 'method3' inClass: class inCategory: '*yyyyy'.
		
	class category: 'YYYYY'.
	
	self assert: (YPackage includesDefinedSelector: #method1 ofClass: class ). 
	self deny: (XPackage includesSelector: #method1 ofClass: class ). 
	self deny: (ZPackage includesSelector: #method1 ofClass: class ). 
	
	self deny: (XPackage includesSelector: #method2 ofClass: class ). 
	self deny: (YPackage includesSelector: #method2 ofClass: class ). 
	self assert: (ZPackage includesExtensionSelector: #method2 ofClass: class ). 

	self deny: (XPackage includesSelector: #method3 ofClass: class ). 
	self assert: (YPackage includesDefinedSelector: #method3 ofClass: class ). 
	self deny: (ZPackage includesSelector: #method3 ofClass: class ). 

]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUnregisterTheClassFromTheOldPackage [
	"test that when we recategorize a class, the old package in which it was defined don't include it anymore"
	

	|XPackage YPackage class|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	class category: 'YYYYY'.
	self deny: (XPackage includesClass: class).
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage [
	"test that when we recategorize a class, the old package in which it was defined don't include its defined methods (not extensions) anymore"	
		
	|XPackage YPackage ZPackage class|
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'method1' inClass: class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class inCategory: '*zzzzz'.
	self createMethodNamed: 'method3' inClass: class inCategory: '*yyyyy'.
	
	class category: 'YYYYY'.
	self deny: (XPackage includesSelector: #method1 ofClass: class ). 
	self deny: (XPackage includesSelector: #method2 ofClass: class ). 
	self deny: (XPackage includesSelector: #method3 ofClass: class ).
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassUpdateTheOrganizerMappings [
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"
	
	|XPackage YPackage class|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	class category: 'YYYYY'.	
	self assert: (self organizer packageOf: class) equals: YPackage		
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassWithMetaClassMethodsRegisterAllClassMethodsInTheNewPackage [

	"test that when we recategorize a class (having methods defined in both instance and class side), the new package in which it is defined include all the methods (from instance and class side) defined in this class (not extensions)"
	
	| XPackage YPackage ZPackage class|
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'method1' inClass: class classSide inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class classSide inCategory: '*yyyyy'.
	self createMethodNamed: 'method3' inClass: class classSide inCategory: '*zzzzz'.
	
	class category: 'YYYYY'.
	
	"lets check metaclass methods"
	self assert: (YPackage includesDefinedSelector: #method1 ofMetaclassName: class name).
	
	self assert: (YPackage includesDefinedSelector: #method2 ofMetaclassName: class name). 
	
	self assert: (ZPackage includesExtensionSelector: #method3 ofMetaclassName: class name ).
]

{ #category : #'tests - recategorizing class' }
RPackageClassesSynchronisationTest >> testRecategorizeClassWithUnexistingPackageNameRegisterTheClassInANewPackage [
	"test that when we recategorize a class in category not yet registered in RPackage, a new rPackage is created with the class inside"

	| XPackage YPackage class |
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self assert: (self organizer packageOf: class) equals: XPackage.

	class category: 'YYYYY'.
	YPackage := self organizer packageNamed: #YYYYY.

	self assert: (self organizer packageOf: class) equals: YPackage.
	self deny: (self organizer packageOf: class) equals: XPackage
]

{ #category : #'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassDefinedMethodsFromItsPackage [
	"test that when we remove a class, the class methods defined in the parent package (not extensions) are all removed  from its parent Package"
	
	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'. 
	
	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (XPackage includesSelector: #stubMethod ofClass: class). 
]

{ #category : #'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassExtensionMethodsFromTheCorrespondingPackage [
	"test that when we remove a class, the class extension methods are all removed  from theire corresponding parent Package"
	
	|XPackage YPackage class|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	
	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (YPackage includesSelector: #stubMethod ofClass: class). 
]

{ #category : #'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUnregisterTheClassFromItsPackage [
	"test that when we remove a class, the class is removed from its parent Package"	

	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (XPackage includesClass: class)
]

{ #category : #'tests - removing classes' }
RPackageClassesSynchronisationTest >> testRemoveClassUpdateTheOrganizerMappings [
	"test that when we remove a class, the organizer is updated so that the class is no longer present in the  'classPackageDictionary' dictionary"
	
	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (self organizer includesPackageBackPointerForClass: class)
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassDefinedInTheParentPackage [
	"test that when we rename a class, the classes Defined in the parent package includes the new name"
		
	| XPackage class |
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage includesClassNamed: 'RPackageNewStubClass'). 
	self deny: (XPackage includesClassNamed: 'RPackageOldStubClass').
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'classDefinedSelectors' dictionary of the parent package is updated with the new name. There fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"
	
	| XPackage  class |
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClassName: 'RPackageOldStubClass' asSymbol ).
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateClassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'classExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Therfore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	| XPackage YPackage class |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClassName: 'RPackageOldStubClass' asSymbol ).
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassDefinedSelectorsInTheParentPackage [
	"test that when we rename a class, the  'metaclassDefinedSelectors' dictionary of the parent package is updated with the new name. Ther fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"
	
	| XPackage  class |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class  inCategory: 'classic category'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage  includesDefinedSelector: #stubMethod ofMetaclassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (XPackage  includesDefinedSelector: #stubMethod ofMetaclassName: 'RPackageOldStubClass' asSymbol ).
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateMetaclassExtensionSelectorsInTheExtendingPackages [
	"test that when we rename a class, the  'metaclassExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Ther fore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"
	
	| XPackage YPackage class |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class  inCategory: '*yyyyy'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofMetaclassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (YPackage includesExtensionSelector: #stubMethod ofMetaclassName: 'RPackageOldStubClass' asSymbol ).
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateOrganizerClassExtendingPackagesMapping [
	"test that when we rename a class, the classExtendingPackages dictionary of the organizer is updated with the new name, so that we can access the packages when specifying the new name"
	
	| XPackage YPackage class |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.
	self createMethodNamed: #stubMethod inClass: class inCategory: '*yyyyy'. 
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: ((self organizer extendingPackagesOfClassNamed: 'RPackageNewStubClass' asSymbol) includes: YPackage). 
	self deny: ((self organizer extendingPackagesOfClassNamed: 'RPackageOldStubClass' asSymbol) includes: YPackage).
]

{ #category : #'tests - operations on classes' }
RPackageClassesSynchronisationTest >> testRenameClassUpdateOrganizerClassPackageMapping [
	"test that when we rename a class, the classPackage dictionary of the organizer is updated with the new name, so that we can access the package when specifying the new name"

	| XPackage class |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.

	class rename: 'RPackageNewStubClass'.

	self assert: (self organizer packageOfClassNamed: 'RPackageNewStubClass' asSymbol) equals: XPackage.
	self assert: (self organizer packageOfClassNamed: 'RPackageOldStubClass' asSymbol) equals: nil
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByAddingExtensionProtocol [
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."
	
	|class| 
	self addXCategory.
	 
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.  
	class organization addCategory: '*yyyyy' before: nil.
	
	self assert: (self organizer includesPackageNamed: #Yyyyy).
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByAddingNewProtocolDoesNothing [
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."

	| XPackage class |
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.
	class organization addCategory: 'accessing' before: nil.

	self assert: (XPackage includesClass: class).
	self assert: (XPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: XPackage extensionMethods size equals: 0
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRemovingClassicCategoryRemoveMethodsFromTheParentPackage [
	"test that when we reoganized a class by removing a classic category (a category not beginning with '*'), the methods from this category are removed from the parent RPackage of the class"
	
	|XPackage  class| 
		
	self addXCategory.
	XPackage := self organizer packageNamed: 'XXXXX'.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	class removeProtocol: 'classic category'.
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRemovingExtensionCategoryRemoveMethodsFromTheExtendingPackage [
	"test that when we reoganized a class by removing an extension category (a category beginning with '*'), all the methods that were inside are removed from the extending package"
	 
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	
	class removeProtocol: '*yyyyy'.
	
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingClassicCategoryToExtensionCategoryMoveAllMethodsFromParentPackageToExtendingPackage [	 
	"test that when we reoganized a class by renaming a  classic category (a category not beginning with '*') to an extension category, all the methods are moved from the  parent package of the class to the extending package"
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: 'classic category'.
	
	class organization renameCategory: 'classic category' toBe: '*yyyyy'.
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod2 ofClass: class).
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingClassicCategoryToExtensionCategoryMoveMethodsFromParentPackageToExtendingPackage [
	"test that when we reoganized a class by renaming a  classic category (a category not beginning with '*') to an extension category, all the methods are moved from the  parent package of the class to the extending package"

	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	class organization renameCategory: 'classic category' toBe: '*yyyyy'.
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToAnotherExtensionCategoryAddMethodsToTheNewPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to another extension category, all the methods are moved to the new extendingPackage"
	
	|XPackage YPackage ZPackage class| 
	self addXYZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	ZPackage  := self organizer packageNamed: #ZZZZZ.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	class organization renameCategory: '*yyyyy' toBe: '*zzzzz'.
	self assert: (ZPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveAllMethodsFromExtendingPackageToParentPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"
	
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: '*yyyyy'.
	
	class organization renameCategory: '*yyyyy' toBe: 'classic category'.
	
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod2 ofClass: class).
]

{ #category : #'tests - operations on protocols' }
RPackageClassesSynchronisationTest >> testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveMethodsFromExtendingPackageToParentPackage [
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"
	
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	class organization renameCategory: '*yyyyy' toBe: 'classic category'.
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClass: class)
]
