Class {
	#name : 'TraitCommonClassDefinitionPrinterTest',
	#superclass : 'TraitAbstractTest',
	#instVars : [
		'emptySlots'
	],
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'helpers' }
TraitCommonClassDefinitionPrinterTest >> packageTagOfMOP [

	^ '''TraitsV2-Tests-MOP'''
]

{ #category : 'running' }
TraitCommonClassDefinitionPrinterTest >> setUp [

	super setUp.
	emptySlots := ClassDefinitionPrinter displayEmptySlots.
	ClassDefinitionPrinter displayEmptySlots: false
]

{ #category : 'running' }
TraitCommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter displayEmptySlots: emptySlots.
	super tearDown
]
