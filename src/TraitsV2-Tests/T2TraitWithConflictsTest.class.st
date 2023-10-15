"
Unit test for trait conflict scenarios
"
Class {
	#name : 'T2TraitWithConflictsTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2TraitWithConflictsTest >> testAddingSameDependencyIsNotConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T2;
			      traits: { t1 } ].

	t1 compile: 'm1 ^42'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t2 + t1 ].

	self assert: c1 new m1 equals: 42
]

{ #category : 'tests' }
T2TraitWithConflictsTest >> testComplexDependencyIsNotConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 t3 t4 t5 t6 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T2;
			      traits: { t1 } ].
	t3 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T3;
			      traits: { t1 } ].
	t4 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T4;
			      traits: { (t2 + t3) } ].
	t5 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T5;
			      traits: { t3 } ].
	t6 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T6;
			      traits: { (t5 + t4) } ].

	t1 compile: 'm1 ^42'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t6 + t1 ].

	self assert: c1 new m1 equals: 42
]

{ #category : 'tests' }
T2TraitWithConflictsTest >> testDiamonProblemIsNotConflict [

	| t1 t2 c1 t3 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T2;
			      traits: { t1 } ].
	t3 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T3;
			      traits: { t1 } ].

	t1 compile: 'm1 ^42'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t2 + t3 ].

	self assert: (c1 new perform: #m1) equals: 42
]

{ #category : 'tests' }
T2TraitWithConflictsTest >> testDifferentMethodsAreConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :builder | builder name: #T2 ].

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^43'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t1 + t2 ].

	self should: [ c1 new m1 ] raise: Error
]

{ #category : 'tests' }
T2TraitWithConflictsTest >> testNonConflictingTraitsAreNotConflict [

	| t1 t2 c1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :builder | builder name: #T2 ].

	t1 compile: 'm1 ^42'.
	t2 compile: 'm2 ^43'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t1 + t2 ].

	self assert: (c1 new perform: #m1) equals: 42.
	self assert: (c1 new perform: #m2) equals: 43
]

{ #category : 'tests' }
T2TraitWithConflictsTest >> testSameSourceButDifferentMethodsAreConflict [

	<ignoreNotImplementedSelectors: #( m1 )>
	| t1 t2 c1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t2 := self makeTrait: [ :builder | builder name: #T2 ].

	t1 compile: 'm1 ^42'.
	t2 compile: 'm1 ^42'.

	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      slots: #( aSlot );
			      traits: t1 + t2 ].

	self should: [ c1 new m1 ] raise: Error
]
