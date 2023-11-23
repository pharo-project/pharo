Class {
	#name : 'TraitChangesTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitChangesTest >> testGeneratingTheSameTraitDoesNotProduceChanges [

	| t1 builder |
	t1 := self newTrait: #T1 with: {  }.

	builder := ShiftClassBuilder new
		           name: #T1;
		           package: self packageNameForTests;
		           traitComposition: TEmpty;
		           tryToFillOldClass;
		           detectBuilderEnhancer;
		           beTrait.

	builder builderEnhancer validateRedefinition: builder oldClass.

	builder validateSuperclass.
	builder compareWithOldClass.

	self assertEmpty: builder changes
]

{ #category : 'tests' }
TraitChangesTest >> testUpdatingTheSameTraitDoesNotProduceChanges [

	| t1 builder |
	t1 := self newTrait: #T1 with: {  }.

	builder := ShiftClassBuilder new
		           name: #T1;
		           package: self packageNameForTests;
		           traitComposition: TEmpty;
		           beTrait;
		           yourself.

	builder oldClass: t1.

	builder tryToFillOldClass.
	builder detectBuilderEnhancer.
	builder builderEnhancer validateRedefinition: builder oldClass.

	builder validateSuperclass.
	builder compareWithOldClass.

	self assertEmpty: builder changes
]

{ #category : 'tests' }
TraitChangesTest >> testconfigureBuilderWithNameTraitCompositionInstanceVariableNamesPackageName [

	| builder |
	builder := ShiftClassBuilder new
		           name: #T1;
		           beTrait;
		           slotsFromString: 'a b';
		           package: self packageNameForTests.

	self assert: builder slots size equals: 2.
	self assert: builder slots first name equals: 'a'.
	self assert: builder slots second name equals: 'b'
]
