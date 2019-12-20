"
Unit test for correct handling of MC class definitions
"
Class {
	#name : #T2TraitMCDefinitionsTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionHasCorrectString [
	| t2 definition t1 definitionString |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }. 
	t2 := self newTrait: #T2 with: {#a => PropertySlot. #b => PropertySlot } uses: t1.

	definition := t2 asClassDefinition.				
	definitionString := String streamContents: [ :s | definition printDefinitionOn: s ].
 	
 	self assert: (definitionString reject: [ :each | each isSeparator]) equals: (t2 definition reject: [ :each | each isSeparator ]).

]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionHasCorrectStringWithoutSlots [
	| t2 definition t1 definitionString |
	t1 := self newTrait: #T1 with: {}. 
	t2 := self newTrait: #T2 with: {#a. #b } uses: t1.

	definition := t2 asClassDefinition.				
	definitionString := String streamContents: [ :s | definition printDefinitionOn: s ].
		
	self assert: (definitionString reject: [ :each | each isSeparator ]) equals: (t2 definition reject: [ :each | each isSeparator ]).

]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfClassSideTrait [
	
	| t1 definition |
	
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #(aSlot).

	definition := t1 asClassDefinition.
	
	self assert: definition classInstanceVariables equals: {#aSlot => InstanceVariableSlot}.

]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfClassSideTraitHasCorrectString [
	
	| t1 definition |
	
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #(aSlot).

	definition := t1 asClassDefinition.
	
	self assert: definition classDefinitionString equals: t1 class definition 

]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfClassSideTraitWithSpecialSlotHasCorrectString [
	
	| t1 definition |
	
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: {#aSlot => WeakSlot.}.

	definition := t1 asClassDefinition.
	
	self assert: (definition classDefinitionString reject: [ :each | each isSeparator ]) equals: (t1 class definition reject: [ :each | each isSeparator ])

]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfNormalClass [
	| c1 definition |
	c1 := self newClass: #C1 with: #(a b c) uses: {}.
	definition := c1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: '{}'.	
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfNormalClassWithSlots [
	| c1 definition |
	c1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } uses: {}.
	definition := c1 asClassDefinition.
		
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: '{}'.	
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTrait [
	| t1 definition |
	t1 := self newTrait: #T1 with: #(a b c) uses: {}.
	definition := t1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: '{}'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTraitUsingTrait [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z). 
	t2 := self newTrait: #T2 with: #(a b c) uses: t1.
	definition := t2 asClassDefinition.
		
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTraitUsingTraitWithSlots [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }. 
	t2 := self newTrait: #T2 with: {#a => PropertySlot. #b => PropertySlot } uses: t1.

	definition := t2 asClassDefinition.	
				
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: 'T1'.	
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTraitWithSlots [
	| t1  definition |
	t1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } uses: {}.

	definition := t1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTraitedClass [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z). 
	c1 := self newClass: #C1 with: #(a b c) uses: t1.
	definition := c1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testDefinitionOfTraitedClassWithSlots [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }. 
	c1 := self newClass: #C1 with: {#a => PropertySlot. #b => PropertySlot } uses: t1.

	definition := c1 asClassDefinition.	
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: 'T1'.	
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testEqualityOfTraitDefinition [
	| t1 definition1 definition2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	definition1 := t1 asClassDefinition.

	t1 := self newTrait: #T1 with: #(a b c) uses: {}.
	definition2 := t1 asClassDefinition.

	self deny: definition1 equals: definition2
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testEqualityOfTraitDefinitionInUses [
	| t1 t2 definition1 definition2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	definition1 := t2 asClassDefinition.

	t2 := self newTrait: #T2 with: #() uses: {t1}.
	definition2 := t2 asClassDefinition.

	self deny: definition1 equals: definition2
]

{ #category : #tests }
T2TraitMCDefinitionsTest >> testEqualityOfTraitDefinitionWithDifferentSlotTypes [
	| t1 definition1 definition2 |
	t1 := self newTrait: #T1 with: #(a) uses: {}.
	definition1 := t1 asClassDefinition.

	t1 := self newTrait: #T1 with: {(#a => PropertySlot)} uses: {}.
	definition2 := t1 asClassDefinition.

	self deny: definition1 equals: definition2
]
