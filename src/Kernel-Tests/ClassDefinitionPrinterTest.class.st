Class {
	#name : #ClassDefinitionPrinterTest,
	#superclass : #CommonClassDefinitionPrinterTest,
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #tests }
ClassDefinitionPrinterTest >> testFluidConfiguration [

	self 
		assert: (ClassDefinitionPrinter fluid
 						for: Array;
						definitionString)
		equals: 'ArrayedCollection < #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
]

{ #category : #tests }
ClassDefinitionPrinterTest >> testLegacyPharoConfiguration [

	self 
		assert: (ClassDefinitionPrinter legacy
 						for: Array;
						definitionString)
		equals: 'ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Collections-Sequenceable-Base'''
]

{ #category : #tests }
ClassDefinitionPrinterTest >> testOldPharoConfiguration [

	self 
		assert: (ClassDefinitionPrinter oldPharo
 						for: Array;
						definitionString)
		equals: 'ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Collections-Sequenceable-Base'''
]

{ #category : #tests }
ClassDefinitionPrinterTest >> testToggleIsWorking [

	| value |
	value := ClassDefinitionPrinter showFluidClassDefinition.
	ClassDefinitionPrinter toggleShowFluidClassDefinition.
	self assert: ClassDefinitionPrinter showFluidClassDefinition equals: value not. 
	
]
