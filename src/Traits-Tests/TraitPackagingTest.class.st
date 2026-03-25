Class {
	#name : 'TraitPackagingTest',
	#superclass : 'TestCase',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitPackagingTest >> testSortClassAndUsedTraitPutsTraitFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          MOPTestClassB.
			          Trait1 }.

	self assert: sorted asArray equals: {
			Trait1.
			MOPTestClassB }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUsedTraitPutsUsedTraitFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait3.
			          Trait2 }.

	self assert: sorted asArray equals: {
			Trait2.
			Trait3 }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUserClassPutsTraitFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait1.
			          MOPTestClassB }.

	self assert: sorted asArray equals: {
			Trait1.
			MOPTestClassB }
]

{ #category : 'tests' }
TraitPackagingTest >> testSortTraitAndUserTraitPutsUsedTraitFirst [

	| sorted |
	sorted := Package new sortTopologically: {
			          Trait3.
			          Trait2 }.

	self assert: sorted asArray equals: {
			Trait2.
			Trait3 }
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
