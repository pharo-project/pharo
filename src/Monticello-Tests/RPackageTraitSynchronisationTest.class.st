"
SUnit tests for RPackage trait synchronization

"
Class {
	#name : 'RPackageTraitSynchronisationTest',
	#superclass : 'RPackageMCSynchronisationTest',
	#category : 'Monticello-Tests-RPackage',
	#package : 'Monticello-Tests',
	#tag : 'RPackage'
}

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testAddMethodByUsingATraitDoesNotAddTheMethodToThePackage [
	"test that when a method is added to a class bu using a trait, this method is not imported in the parent package of the class"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'trait category'.

	self assertEmpty: xPackage methods.
	self denyEmpty: yPackage methods.
	class addToComposition: trait.
	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self assertEmpty: xPackage methods
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitAddItIntoPackageBestMatchName [

	| tmpPackage class |
	tmpPackage := self ensureXPackage.
	self ensureTagYinX.

	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX-YYYY'.

	self assert: (tmpPackage definesClass: class).
	self assert: tmpPackage equals: class package
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitAddItIntoPackageExactName [

	| tmpPackage class |
	tmpPackage := self ensureXPackage.
	self ensureTagYinX.

	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.

	self assert: (tmpPackage definesClass: class).
	self assert: tmpPackage equals: class package
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitUpdateTheOrganizerMappings [
	"test that when we add a Trait, the organizer 'classPackageMapping' dictionary is updated, so that the trait is linked to its package and so that we can access its owning package"

	| tmpPackage class |
	tmpPackage := self ensureXPackage.

	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.

	self assert: class package equals: tmpPackage
]

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testRemoveMethodComingFromTraitDoesNotRemoveMethodFromTraitPackage [
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	class removeSelector: #stubMethod.
	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testRemoveTraitMethod [
	"test that when we remove a method from a trait used by a class, the method is removed from the trait package normally"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.
	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	trait removeSelector: #stubMethod.
	self deny: (yPackage includesSelector: #stubMethod ofClass: trait).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testRemoveTraitMethodOverridenByClassDoesRemoveMethodFromClassPackage [
	"test that when we remove a class method overriding the one from a trait, the method is removed from the class package"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic protocol'.

	class removeSelector: #stubMethod.
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testTraitCompositionMethodsArePackagedWithTheirTraits [
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]
