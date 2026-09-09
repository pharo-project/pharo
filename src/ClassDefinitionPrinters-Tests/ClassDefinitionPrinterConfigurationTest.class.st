Class {
	#name : 'ClassDefinitionPrinterConfigurationTest',
	#superclass : 'CommonClassDefinitionPrinterTest',
	#category : 'ClassDefinitionPrinters-Tests',
	#package : 'ClassDefinitionPrinters-Tests'
}

{ #category : 'tests' }
ClassDefinitionPrinterConfigurationTest >> testFluidConfiguration [

	self
		assert: (ClassDefinitionPrinter fluid
 						for: Array;
						definitionString)
		equals: 'ArrayedCollection << #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
]

{ #category : 'tests' }
ClassDefinitionPrinterConfigurationTest >> testLegacyPharoConfiguration [

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

{ #category : 'tests' }
ClassDefinitionPrinterConfigurationTest >> testOldPharoConfiguration [

	self
		assert: (ClassDefinitionPrinter oldPharo
 						for: Array;
						definitionString)
		equals: 'ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Collections-Sequenceable-Base'''
]
