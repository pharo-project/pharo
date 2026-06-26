Class {
	#name : 'TraitMethodToCompileInfoTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> check: info selector: selector protocol: protocol definingTrait: aTraitComposition method: method sourceCode: code [

	self
		check: info
		selector: selector
		protocol: protocol
		definingTrait: aTraitComposition
		sourceCode: code.
	self assert: info compiledMethod equals: method
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> check: info selector: selector protocol: protocol definingTrait: aTraitComposition sourceCode: code [

	self
		check: info
		selector: selector
		protocol: protocol
		sourceCode: code
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> check: info selector: selector protocol: protocol sourceCode: code [

	self assert: info selector equals: selector.
	self assert: info protocol equals: protocol.
	self assert: info sourceCode equals: code
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testDiamond [

	| c1 t1 t2 t3 infos |
	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^1' classified: #proto.

	t2 := self newTrait: #T2 traits: t1.
	t3 := self newTrait: #T3 traits: t1.
	c1 := self newClass: #C1 traits: t2 + t3.


	infos := c1 traitComposition methodsCompilationInfo sorted: #selector ascending.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: t2 traitComposition
		method: t1 >> #m1
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

	infos := c1 traitComposition methodsCompilationInfo sorted: #selector ascending.

	self assert: infos size equals: 2.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: c1 traitComposition members first
		method: t1 >> #m1
		sourceCode: 'm1 ^1'.
	self
		check: infos second
		selector: #m2
		protocol: #proto
		definingTrait: c1 traitComposition members second
		method: t2 >> #m2
		sourceCode: 'm2 ^2'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testSequenceDoesNotConflictOnRequirement [

	| c1 t1 t2 infos |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1 + t2.

	t1 compile: 'm1 self explicitRequirement' classified: #proto2.
	t2 compile: 'm1 ^2' classified: #proto.

	infos := c1 traitComposition methodsCompilationInfo.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: c1 traitComposition members second
		sourceCode: 'm1 ^2'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testSequenceWithConflict [

	| c1 t1 t2 infos |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1 + t2.

	t1 compile: 'm1 ^1' classified: #proto.
	t2 compile: 'm1 ^2' classified: #proto2.

	infos := c1 traitComposition methodsCompilationInfo sorted: #selector ascending.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: Protocol traitConflictName
		definingTrait: c1 traitComposition members first
		sourceCode: 'm1
	self traitConflict'
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testSequenceWithTwoRequirements [

	| c1 t1 t2 infos |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1 + t2.

	t1 compile: 'm1 self explicitRequirement' classified: #proto2.
	t2 compile: 'm1 self explicitRequirement' classified: #proto.

	infos := c1 traitComposition methodsCompilationInfo.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: Protocol traitConflictName
		sourceCode: 'm1 self explicitRequirement'.
	self assert: (c1 traitComposition members includes: infos first definingTrait)
]

{ #category : 'tests' }
TraitMethodToCompileInfoTest >> testUniqueTrait [

	| c1 t1 infos |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	t1 compile: 'm1 ^1' classified: #proto.

	infos := c1 traitComposition methodsCompilationInfo.

	self assert: infos size equals: 1.
	self
		check: infos first
		selector: #m1
		protocol: #proto
		definingTrait: c1 traitComposition
		method: t1 >> #m1
		sourceCode: 'm1 ^1'
]
