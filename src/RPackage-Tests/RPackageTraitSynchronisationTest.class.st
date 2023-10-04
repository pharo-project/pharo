"
SUnit tests for RPackage trait synchronization

"
Class {
	#name : 'RPackageTraitSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testAddMethodByUsingATraitDoesNotAddTheMethodToThePackage [
	"test that when a method is added to a class bu using a trait, this method is not imported in the parent package of the class"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'trait category'.

	self assertEmpty: xPackage methods.
	self denyEmpty: yPackage methods.
	class addToComposition: trait.
	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self assertEmpty: xPackage methods
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitAddItIntoPackageBestMatchName [

	| xPackage tag class |
	tag := self ensureTagYinX.
	xPackage := tag package.

	class := self newTraitNamed: 'NewClass' inTag: tag.

	self assert: (xPackage definesClass: class).
	self assert: xPackage equals: class package
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitAddItIntoPackageExactName [

	| xPackage class |
	xPackage := self ensureXPackage.
	self ensureTagYinX.

	class := self newTraitNamed: 'NewClass' in: xPackage.

	self assert: (xPackage definesClass: class).
	self assert: xPackage equals: class package
]

{ #category : 'tests - operations on traits' }
RPackageTraitSynchronisationTest >> testAddTraitUpdateTheOrganizerMappings [
	"test that when we add a Trait, the organizer 'classPackageMapping' dictionary is updated, so that the trait is linked to its package and so that we can access its owning package"

	| xPackage class |
	xPackage := self ensureXPackage.

	class := self newTraitNamed: 'NewClass' in: xPackage.

	self assert: class package equals: xPackage
]

{ #category : 'tests - operations on methods' }
RPackageTraitSynchronisationTest >> testRemoveMethodComingFromTraitDoesNotRemoveMethodFromTraitPackage [
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

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
	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.
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

	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

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

	class := self newClassNamed: 'NewClass' in: yPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class)
]
