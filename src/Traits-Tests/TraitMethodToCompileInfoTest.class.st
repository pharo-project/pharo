Class {
	#name : 'TraitMethodToCompileInfoTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> check: info selector: selector protocol: protocol definingTrait: aTraitComposition sourceCode: code [

	self assert: info selector equals: selector.
	self assert: info protocol equals: protocol.
	self assert: info compiledMethod equals: aTraitComposition >> selector.
	self assert: info definingTrait equals: aTraitComposition.
	self flag: #todo "Add source code"
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testDiamond [

	| c1 t1 t2 t3 infos |
	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^1' classified: #proto.
	
	t2 := self newTrait: #T2 traits: t1.
	t3 := self newTrait: #T3 traits: t1.
	c1 := self newClass: #C1 traits: t2 + t3.


	infos := c1 traitComposition compilationInfoForSelectors sorted: #selector ascending.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: t2 traitComposition
		sourceCode: 'm1 ^1'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testSequence [

	| c1 t1 t2 infos |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1 + t2.

	t1 compile: 'm1 ^1' classified: #proto.
	t2 compile: 'm2 ^2' classified: #proto.

	infos := c1 traitComposition compilationInfoForSelectors sorted: #selector ascending.

	self assert: infos size equals: 2.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: c1 traitComposition members first
		sourceCode: 'm1 ^1'.
	self
		check: infos second
		selector: #m2
		protocol: #proto
		definingTrait: c1 traitComposition members second
		sourceCode: 'm2 ^2'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testSequenceWithConflict [

	| c1 t1 t2 infos |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1 + t2.

	t1 compile: 'm1 ^1' classified: #proto.
	t2 compile: 'm1 ^1' classified: #proto.

	infos := c1 traitComposition compilationInfoForSelectors sorted: #selector ascending.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: Protocol traitConflictName
		definingTrait: c1 traitComposition members first
		sourceCode: 'm1 ^ self traitConflict'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testUniqueTrait [

	| c1 t1 infos |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	t1 compile: 'm1 ^1' classified: #proto.

	infos := c1 traitComposition compilationInfoForSelectors.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: c1 traitComposition
		sourceCode: 'm1 ^1'
]
