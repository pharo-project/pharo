Class {
	#name : #T2TraitChangesTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests-Tests'
}

{ #category : #tests }
T2TraitChangesTest >> testGeneratingTheSameTraitDoesNotProduceChanges [
	
	| t1 builder | 
	
	t1 := self newTrait: #T1 with: {}. 
	
	builder := ShiftClassBuilder new.
	builder buildEnvironment: ShSmalltalkGlobalsEnvironment new.

	Trait configureBuilder: builder withName: #T1 traitComposition: {} slots: {}  packageName: 'TraitsV2-Tests-TestClasses'.
				
	builder tryToFillOldClass.
	builder detectBuilderEnhancer.
	builder builderEnhancer validateRedefinition: builder oldClass. 
	
	builder validateSuperclass.
	builder compareWithOldClass.
	
	self assertEmpty: builder changes.
]

{ #category : #tests }
T2TraitChangesTest >> testUpdatingTheSameTraitDoesNotProduceChanges [
	
	| t1 builder | 
	
	t1 := self newTrait: #T1 with: {}. 
	
	builder := ShiftClassBuilder new.
	builder buildEnvironment: ShSmalltalkGlobalsEnvironment new.

	Trait configureBuilder: builder withName: #T1 traitComposition: {} slots: {}  packageName: 'TraitsV2-Tests-TestClasses'.

	builder oldClass: t1.
				
	builder tryToFillOldClass.
	builder detectBuilderEnhancer.
	builder builderEnhancer validateRedefinition: builder oldClass. 
	
	builder validateSuperclass.
	builder compareWithOldClass.
	
	self assertEmpty: builder changes.
]
