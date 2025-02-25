Class {
	#name : 'TraitCommonClassDefinitionPrinterTest',
	#superclass : 'TraitAbstractTest',
	#instVars : [
		'emptySlots'
	],
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'helpers' }
TraitCommonClassDefinitionPrinterTest >> packageTagOfMOP [

	^ '''Traits-Tests-MOP'''
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
