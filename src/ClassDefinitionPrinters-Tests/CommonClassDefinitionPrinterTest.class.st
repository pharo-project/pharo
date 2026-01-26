"
I do not test the factory logic of ClassDefinitionPrinter because it is done in a companion test class.
Here I'm just a way to share code between all the tests.
"
Class {
	#name : 'CommonClassDefinitionPrinterTest',
	#superclass : 'TestCase',
	#instVars : [
		'slotDisplay'
	],
	#category : 'ClassDefinitionPrinters-Tests',
	#package : 'ClassDefinitionPrinters-Tests'
}

{ #category : 'running' }
CommonClassDefinitionPrinterTest >> setUp [

	super setUp.
	slotDisplay := ClassDefinitionPrinter displayEmptySlots.
	ClassDefinitionPrinter displayEmptySlots: false
]

{ #category : 'running' }
CommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter displayEmptySlots: slotDisplay.
	super tearDown
]
