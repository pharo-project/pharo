Class {
	#name : 'TraitPackagingTest',
	#superclass : 'TestCase',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitPackagingTest >> testSortClassAndUsedTraitPutsClassFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          MOPTestClassB.
			          Trait1 }.

	self assert: sorted asArray equals: {
			MOPTestClassB.
			Trait1 }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUsedTraitPutsUserFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait2.
			          Trait3 }.

	self assert: sorted asArray equals: {
			Trait3.
			Trait2 }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUserClassPutsClassFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait1.
			          MOPTestClassB }.

	self assert: sorted asArray equals: {
			MOPTestClassB.
			Trait1 }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUserTraitPutsUserFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait3.
			          Trait2 }.

	self assert: sorted asArray equals: {
			Trait3.
			Trait2 }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortUnrelatedClassAndTraitDoesNotMatterOrder [

	| sorted |
	sorted := Package new sortTopologically: {
			          TraitPackagingTest.
			          Trait1 }.

	self assertCollection: sorted asSet hasSameElements: {
			TraitPackagingTest.
			Trait1 }
]
