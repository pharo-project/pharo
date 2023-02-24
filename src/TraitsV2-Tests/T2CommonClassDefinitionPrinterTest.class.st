Class {
	#name : #T2CommonClassDefinitionPrinterTest,
	#superclass : #T2AbstractTest,
	#instVars : [
		'slotuse',
		'emptySlots'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> setUp [
	super setUp.
	slotuse := ClassDefinitionPrinter showFluidClassDefinition.
	emptySlots := ClassDefinitionPrinter displayEmptySlots.
	ClassDefinitionPrinter displayEmptySlots: false
]

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter displayEmptySlots: emptySlots.
	ClassDefinitionPrinter showFluidClassDefinition: slotuse.
	super tearDown
]
