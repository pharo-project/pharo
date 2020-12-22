"
Unit test for trait conflict scenarios
"
Class {
	#name : #T2TraitWithConflictsTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitWithConflictsTest >> testAddingSameDependencyIsNotConflict [
	| t1 t2 c1 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {t1}.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t2 + t1.
	
	self assert:c1 new m1 equals: 42.

]

{ #category : #tests }
T2TraitWithConflictsTest >> testComplexDependencyIsNotConflict [
	| t1 t2 c1 t3 t4 t5 t6|
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {t1}.
	t3 := self newTrait: #T3 with: #() uses: {t1}.
	t4 := self newTrait: #T4 with: #() uses: {t2 + t3}.
	t5 := self newTrait: #T5 with: #() uses: {t3}.
	t6 := self newTrait: #T6 with: #() uses: {t5 + t4}.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t6 + t1.
	
	self assert:c1 new m1 equals: 42.

]

{ #category : #tests }
T2TraitWithConflictsTest >> testDiamonProblemIsNotConflict [
	| t1 t2 c1 t3 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {t1}.
	t3 := self newTrait: #T3 with: #() uses: {t1}.

	t1 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t2 + t3.
		
	self assert: (c1 new perform: #m1) equals: 42.

]

{ #category : #tests }
T2TraitWithConflictsTest >> testDifferentMethodsAreConflict [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^43'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.
	
	self should: [ c1 new m1 ] raise: Error

]

{ #category : #tests }
T2TraitWithConflictsTest >> testNonConflictingTraitsAreNotConflict [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm2 ^43'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.
	
	self assert: (c1 new perform: #m1) equals: 42.
	self assert: (c1 new perform: #m2) equals: 43.

]

{ #category : #tests }
T2TraitWithConflictsTest >> testSameSourceButDifferentMethodsAreConflict [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^42'.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.
	
	self should: [c1 new m1] raise: Error

]
