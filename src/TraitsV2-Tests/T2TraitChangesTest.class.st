Class {
	#name : 'T2TraitChangesTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2TraitChangesTest >> testGeneratingTheSameTraitDoesNotProduceChanges [

	| t1 builder |

	t1 := self newTrait: #T1 with: {}.

	builder := ShiftClassBuilder new
		name: #T1;
		package: 'TraitsV2-Tests-TestClasses';
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
T2TraitChangesTest >> testUpdatingTheSameTraitDoesNotProduceChanges [

	| t1 builder |
	t1 := self newTrait: #T1 with: {  }.

	builder := ShiftClassBuilder new
		           name: #T1;
		           package: 'TraitsV2-Tests-TestClasses';
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
T2TraitChangesTest >> testconfigureBuilderWithNameTraitCompositionInstanceVariableNamesPackageName [

	| builder |
	builder := ShiftClassBuilder new
		           name: #T1;
		           beTrait;
		           slotsFromString: 'a b';
		           package: 'TraitsV2-Tests-TestClasses'.

	self assert: builder slots size equals: 2.
	self assert: builder slots first name equals: 'a'.
	self assert: builder slots second name equals: 'b'
]
