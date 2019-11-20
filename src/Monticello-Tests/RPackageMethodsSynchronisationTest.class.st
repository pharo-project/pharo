"
SUnit tests for RPackage method synchronization
"
Class {
	#name : #RPackageMethodsSynchronisationTest,
	#superclass : #RPackageMCSynchronisationTest,
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsClass [
	"test that when we add a method to a  class in a classic category (not beginning with *), this method is added to the parent package of the class"
	
	|tmpPackage class|
	self addXCategory.
	
	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic category'.

	self assert: (class package includesDefinedSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) equals: class package.
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsTrait [
	"test that when we add a method to a  trait in a classic category (*not beginning with *), this method is added to the parent package of the class"

	| tmpPackage trait |
	self addXCategory.

	tmpPackage := self organizer packageNamed: #XXXXX.
	trait := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic category'.

	self assert: (trait package includesDefinedSelector: #stubMethod ofClass: trait).
	self assert: (trait >> #stubMethod packageFromOrganizer: self organizer) equals: trait package
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyMethodByMovingFromClassicCategoryToClassicCategoryDoesNothing [
	"test that when we move a method from a classic category (not begining with *) to another classic category , the packaging keeps the same"

	| class firstPackage |
	self addXCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	"this we do"
	class organization classify: #stubMethod under: 'new category'.

	"this we check"
	self assert: (class >> #stubMethod) category equals: 'new category'.
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: firstPackage
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyMethodByMovingFromClassicCategoryToExtensionCategoryMoveItFromClassPackageToExtendingPackage [
	"test that when we move a method from a classic category (not begining with *) to an extension category , the method is moved from the parent package of the class to the extending package"

	| class firstPackage secondPackage |
	self addXYCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class organization classify: #stubMethod under: '*yyyyy'.
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage.

	class organization classify: #stubMethod under: '*yyyyy-subcategory'.
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyMethodByMovingFromExtensionCategoryToClassicCategoryMoveItFromExtendingPackageToClassPackage [
	"test that when we move a method from an extension category ( begining with *) to a classic category , the method is moved from the extending package to the parent package of the class"

	| class firstPackage secondPackage |
	self addXYCategory.

	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class organization classify: #stubMethod under: 'classic category'.
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: firstPackage
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyMethodByMovingFromExtensionCategoryToExtensionCategoryMoveItFromExtendingPackageToSecondExtendingPackage [
	"test that when we move a method from an extension category ( begining with *) to another extending package , the method is moved from the extending package to the other extending package"

	| class XPackage YPackage ZPackage |
	self addXYZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: #newMethod inClass: class inCategory: '*yyyyy'.

	class organization classify: #newMethod under: '*zzzzz'.
	self deny: (XPackage includesDefinedSelector: #newMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (ZPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (class >> #newMethod packageFromOrganizer: self organizer) equals: ZPackage
]

{ #category : #'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testRemoveMethodRemoveMethodFromItsPackage [
	"test that when we remove a method, the method is remod from the package in which the method is defined"
	
	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'. 
	
	class removeSelector: #stubMethod.
	
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class). 
	self deny: (XPackage includesExtensionSelector: #stubMethod ofClass: class). 
	self deny: (XPackage includesSelector: #stubMethod ofClass: class).
]
