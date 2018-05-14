Class {
	#name : #T2TraitMCDefinitions,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfNormalClass [
	| c1 definition |
	c1 := self newClass: #C1 with: #(a b c) uses: {}.
	definition := c1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfNormalClassWithSlots [
	| c1 definition |
	c1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } uses: {}.
	definition := c1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTrait [
	| t1 definition |
	t1 := self newTrait: #T1 with: #(a b c) uses: {}.
	definition := t1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTraitUsingTrait [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z). 
	t2 := self newClass: #T2 with: #(a b c) uses: t1.
	definition := t2 asClassDefinition.
		
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTraitUsingTraitWithSlots [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }. 
	t2 := self newTrait: #T2 with: {#a => PropertySlot. #b => PropertySlot } uses: t1.

	definition := t2 asClassDefinition.	
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTraitWithSlots [
	| t1  definition |
	t1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } uses: {}.
	definition := t1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTraitedClass [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z). 
	c1 := self newClass: #C1 with: #(a b c) uses: t1.
	definition := c1 asClassDefinition.
	
	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : #tests }
T2TraitMCDefinitions >> testDefinitionOfTraitedClassWithSlots [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }. 
	c1 := self newClass: #C1 with: {#a => PropertySlot. #b => PropertySlot } uses: t1.

	definition := c1 asClassDefinition.	
	
	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]
