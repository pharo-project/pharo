"
Unit test for correct handling of MC class definitions
"
Class {
	#name : 'TraitMCDefinitionsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionHasCorrectString [

	| t2 definition t1 definitionString |
	t1 := self newTrait: #T1 with: {
			      (#x => PropertySlot).
			      (#y => PropertySlot) }.
	t2 := self
		      newTrait: #T2
		      with: {
				      (#a => PropertySlot).
				      (#b => PropertySlot) }
		      traits: t1.

	definition := t2 asClassDefinition.
	definitionString := String streamContents: [ :s | definition printDefinitionOn: s ].

	self
		assert: (definitionString reject: [ :each | each isSeparator ])
		equals: 'Traitnamed:#T2uses:T1instanceVariableNames:''#a=>PropertySlot#b=>PropertySlot''package:''' , self packageNameForTests , ''''
	"We cannot use the class printer because this is a mix between instancevariableNames and complex slot."
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionHasCorrectStringWithoutSlots [

	| t2 definition t1 definitionString |
	t1 := self newTrait: #T1 with: {  }.
	t2 := self newTrait: #T2 with: { #a. #b } traits: t1.

	definition := t2 asClassDefinition.
	definitionString := String streamContents: [ :s | definition printDefinitionOn: s ].

	self
		assert: (definitionString reject: [ :each | each isSeparator ])
		equals: 'Traitnamed:#T2uses:T1instanceVariableNames:''ab''package:''' , self packageNameForTests , ''''


	"We cannot use ((ClassDefinitionPrinter oldPharo for: t2) definitionString reject: [ :each | each isSeparator ])
	because slots is still used in the oldPharo printer implementation and this should change."
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfClassSideTrait [

	| t1 definition |
	t1 := self newTrait: #T1.
	t1 classTrait trait: {  } slots: #( aSlot ).

	definition := t1 asClassDefinition.

	self
		assert: definition classInstanceVariables
		equals: { (#aSlot => InstanceVariableSlot) }
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfClassSideTraitHasCorrectString [

	| t1 definition |
	t1 := self newTrait: #T1.
	t1 classTrait trait: { } slots: #(aSlot).

	definition := t1 asClassDefinition.

	self assert: definition classDefinitionString equals: 'T1 classTrait instanceVariableNames: ''aSlot'''

	"We cannot use the class printer because this is a mix between instancevariableNames and complex slot."
	"(ClassDefinitionPrinter oldPharo for: t1 class) definitionString "
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfClassSideTraitWithSpecialSlotHasCorrectString [

	| t1 definition |

	t1 := self newTrait: #T1.
	t1 classTrait trait: { } slots: {#aSlot => WeakSlot.}.

	definition := t1 asClassDefinition.

	self assert: (definition classDefinitionString reject: [ :each | each isSeparator ]) equals: 'T1classTraitinstanceVariableNames:''#aSlot=>WeakSlot'''
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfNormalClass [
	| c1 definition |
	c1 := self newClass: #C1 with: #(a b c) traits: {}.
	definition := c1 asClassDefinition.

	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: '{}'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfNormalClassWithSlots [
	| c1 definition |
	c1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } traits: {}.
	definition := c1 asClassDefinition.

	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: '{}'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTrait [
	| t1 definition |
	t1 := self newTrait: #T1 with: #(a b c) traits: {}.
	definition := t1 asClassDefinition.

	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: '{}'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTraitUsingTrait [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z).
	t2 := self newTrait: #T2 with: #(a b c) traits: t1.
	definition := t2 asClassDefinition.

	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTraitUsingTraitWithSlots [
	| t2 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }.
	t2 := self newTrait: #T2 with: {#a => PropertySlot. #b => PropertySlot } traits: t1.

	definition := t2 asClassDefinition.

	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTraitWithSlots [
	| t1  definition |
	t1 := self newClass: #C1 with: { #a => PropertySlot. #b => PropertySlot } traits: {}.

	definition := t1 asClassDefinition.

	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTraitedClass [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: #(x y z).
	c1 := self newClass: #C1 with: #(a b c) traits: t1.
	definition := c1 asClassDefinition.

	self assert: definition instanceVariablesString equals: 'a b c'.
	self assert: definition instanceVariables size equals: 3.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = InstanceVariableSlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testDefinitionOfTraitedClassWithSlots [
	| c1 definition t1 |
	t1 := self newTrait: #T1 with: {#x => PropertySlot. #y => PropertySlot }.
	c1 := self newClass: #C1 with: {#a => PropertySlot. #b => PropertySlot } traits: t1.

	definition := c1 asClassDefinition.

	self assert: definition instanceVariablesString equals: '#a => PropertySlot #b => PropertySlot'.
	self assert: definition instanceVariables size equals: 2.
	self assert: definition traitComposition equals: 'T1'.
	self assert: (definition instanceVariables allSatisfy: [ :e | e species = PropertySlot ])
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testEqualityOfTraitDefinition [
	| t1 definition1 definition2 |
	t1 := self newTrait: #T1.
	definition1 := t1 asClassDefinition.

	t1 := self newTrait: #T1 with: #(a b c).
	definition2 := t1 asClassDefinition.

	self deny: definition1 equals: definition2
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testEqualityOfTraitDefinitionInUses [
	| t1 t2 definition1 definition2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	definition1 := t2 asClassDefinition.

	t2 := self newTrait: #T2 traits: {t1}.
	definition2 := t2 asClassDefinition.

	self deny: definition1 equals: definition2
]

{ #category : 'tests' }
TraitMCDefinitionsTest >> testEqualityOfTraitDefinitionWithDifferentSlotTypes [

	| t1 definition1 definition2 |
	t1 := self newTrait: #T1 with: #( a ).
	definition1 := t1 asClassDefinition.

	t1 := self newTrait: #T1 with: { (#a => PropertySlot) }.
	definition2 := t1 asClassDefinition.

	self deny: definition1 equals: definition2
]
