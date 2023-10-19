Class {
	#name : 'T2CommonClassDefinitionPrinterTest',
	#superclass : 'T2AbstractTest',
	#instVars : [
		'emptySlots'
	],
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'helpers' }
T2CommonClassDefinitionPrinterTest >> packageTagOfMOP [

	^ '''TraitsV2-Tests-MOP'''
]

{ #category : 'running' }
T2CommonClassDefinitionPrinterTest >> setUp [

	super setUp.
	emptySlots := ClassDefinitionPrinter displayEmptySlots.
	ClassDefinitionPrinter displayEmptySlots: false
]

{ #category : 'running' }
T2CommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter displayEmptySlots: emptySlots.
	super tearDown
]
