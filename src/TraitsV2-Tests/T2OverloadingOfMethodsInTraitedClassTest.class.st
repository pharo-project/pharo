Class {
	#name : 'T2OverloadingOfMethodsInTraitedClassTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2OverloadingOfMethodsInTraitedClassTest >> createT1 [

	| t1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	t1 classTrait compile: 'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^true
	'.
	TraitedClass compile: 'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^false
	'.
	Behavior compile: 'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^false
	'.
	^ t1
]

{ #category : 'running' }
T2OverloadingOfMethodsInTraitedClassTest >> tearDown [

	TraitedClass removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	Behavior removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	super tearDown
]

{ #category : 'tests' }
T2OverloadingOfMethodsInTraitedClassTest >> testDirectTraitSubclassing [

	<ignoreNotImplementedSelectors: #( isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest )>
	| t1 c1 |
	t1 := self createT1.
	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      traits: t1 ].
	self assert: c1 isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]

{ #category : 'tests' }
T2OverloadingOfMethodsInTraitedClassTest >> testIndirectTraitSubclassing [

	<ignoreNotImplementedSelectors:
	#( isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest )>
	| t1 t2 superclass subclass |
	t1 := self createT1.
	t2 := self makeTrait: [ :builder | builder name: #T2 ].
	superclass := self make: [ :aClassBuilder |
		              aClassBuilder
			              name: #C2Superclass;
			              traits: t2 ].

	subclass := self make: [ :aClassBuilder |
		            aClassBuilder
			            name: #C1Subclass;
			            superclass: superclass;
			            traits: t1 ].
	self deny: superclass isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	self assert: subclass isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]
