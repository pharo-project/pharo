Class {
	#name : #T2CommonClassDefinitionPrinterTest,
	#superclass : #T2AbstractTest,
	#traits : 'THelperTraitTest',
	#classTraits : 'THelperTraitTest classTrait',
	#instVars : [
		'slotuse',
		'emptySlots',
		'traitedTrait'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #removing }
T2CommonClassDefinitionPrinterTest >> removeTraitedTrait [

	traitedTrait removeFromSystem.
]

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> setUp [ 
	super setUp.
	slotuse := ClassDefinitionPrinter showFluidClassDefinition.
	emptySlots := ClassDefinitionPrinter displayEmptySlots.
	ClassDefinitionPrinter displayEmptySlots: false.
	self generateTraitForTests.
	self generateTraitedTrait.
	self generateTestClasses.
]

{ #category : #running }
T2CommonClassDefinitionPrinterTest >> tearDown [

	ClassDefinitionPrinter displayEmptySlots: emptySlots.
	ClassDefinitionPrinter showFluidClassDefinition: slotuse.
	self removeTestClasses.
	self removeTraitForTests.
	super tearDown
]
