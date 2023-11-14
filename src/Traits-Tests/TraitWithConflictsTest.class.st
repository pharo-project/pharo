"
Unit test for trait conflict scenarios
"
Class {
	#name : 'TraitWithConflictsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitWithConflictsTest >> testAddingSameDependencyIsNotConflict [
	| t1 t2 c1 |
	<ignoreNotImplementedSelectors: #(m1)>
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 traits: {t1}.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #(aSlot) traits: t2 + t1.

	self assert:c1 new m1 equals: 42
]

{ #category : 'tests' }
TraitWithConflictsTest >> testComplexDependencyIsNotConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 t3 t4 t5 t6 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 traits: { t1 }.
	t3 := self newTrait: #T3 traits: { t1 }.
	t4 := self newTrait: #T4 traits: { (t2 + t3) }.
	t5 := self newTrait: #T5 traits: { t3 }.
	t6 := self newTrait: #T6 traits: { (t5 + t4) }.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t6 + t1.

	self assert: c1 new m1 equals: 42
]

{ #category : 'tests' }
TraitWithConflictsTest >> testDiamonProblemIsNotConflict [

	| t1 t2 c1 t3 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 traits: { t1 }.
	t3 := self newTrait: #T3 traits: { t1 }.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t2 + t3.

	self assert: (c1 new perform: #m1) equals: 42
]

{ #category : 'tests' }
TraitWithConflictsTest >> testDifferentMethodsAreConflict [
	| t1 t2 c1 |
	<ignoreNotImplementedSelectors: #(m1)>
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^43'.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t2.

	self should: [ c1 new m1 ] raise: Error
]

{ #category : 'tests' }
TraitWithConflictsTest >> testNonConflictingTraitsAreNotConflict [

	| t1 t2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm2 ^43'.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t1 + t2.

	self assert: (c1 new perform: #m1) equals: 42.
	self assert: (c1 new perform: #m2) equals: 43
]

{ #category : 'tests' }
TraitWithConflictsTest >> testSameSourceButDifferentMethodsAreConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t1 + t2.

	self should: [ c1 new m1 ] raise: Error
]

{ #category : 'tests' }
TraitWithConflictsTest >> testTraitCompositionSelectorsDoesNotHaveDuplicates [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 t3 t4 t5 t6 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 traits: { t1 }.
	t3 := self newTrait: #T3 traits: { t1 }.
	t4 := self newTrait: #T4 traits: { (t2 + t3) }.
	t5 := self newTrait: #T5 traits: { t3 }.
	t6 := self newTrait: #T6 traits: { (t5 + t4) }.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t6 + t1.

	{  t2 . t3 . t4 . t5 . t6 . c1 } do: [ :behavior | self assert: behavior traitComposition selectors equals: { #m1 } ].
	self assertEmpty: t1 traitComposition selectors
]
