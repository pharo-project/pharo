Class {
	#name : #T2CommonClassDefinitionPrinterTest,
	#superclass : #T2AbstractTest,
	#instVars : [
		'slotuse'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> setUp [ 
	super setUp.
	slotuse := ClassDefinitionPrinter showFluidClassDefinition.
	
]

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter showFluidClassDefinition: slotuse.
	super tearDown
]
