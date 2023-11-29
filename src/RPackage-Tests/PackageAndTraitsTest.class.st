"
SUnit tests for Package trait synchronization

"
Class {
	#name : 'PackageAndTraitsTest',
	#superclass : 'PackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests - operations on methods' }
PackageAndTraitsTest >> testAddMethodByUsingATraitDoesNotAddTheMethodToThePackage [
	"test that when a method is added to a class bu using a trait, this method is not imported in the parent package of the class"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'trait category'.

	self assertEmpty: xPackage methods.
	self denyEmpty: yPackage methods.
	class addToComposition: trait.
	self assert: (trait >> #stubMethod) package equals: yPackage.
	self assert: (class >> #stubMethod) package equals: yPackage.
	self assertEmpty: xPackage methods
]

{ #category : 'tests - operations on methods' }
PackageAndTraitsTest >> testRemoveMethodComingFromTraitDoesNotRemoveMethodFromTraitPackage [
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	class removeSelector: #stubMethod.
	self assert: (trait >> #stubMethod) package equals: yPackage.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
PackageAndTraitsTest >> testRemoveTraitMethod [
	"test that when we remove a method from a trait used by a class, the method is removed from the trait package normally"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.
	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	trait removeSelector: #stubMethod.
	self deny: (yPackage includesSelector: #stubMethod ofClass: trait).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
PackageAndTraitsTest >> testRemoveTraitMethodOverridenByClassDoesRemoveMethodFromClassPackage [
	"test that when we remove a class method overriding the one from a trait, the method is removed from the class package"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic protocol'.

	class removeSelector: #stubMethod.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
PackageAndTraitsTest >> testTraitCompositionMethodsArePackagedWithTheirTraits [
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"

	| xPackage yPackage class trait |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	trait := self newTraitNamed: 'NewTrait' in: yPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'classic protocol'.
	class setTraitComposition: trait asTraitComposition.

	self assert: (trait >> #stubMethod) package equals: yPackage.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]
