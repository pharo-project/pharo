"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'MockWithComplexSlot',
	#superclass : 'Object',
	#instVars : [
		'#aComplexSlot => ObservableSlot'
	],
	#category : 'ClassDefinitionPrinters-Tests',
	#package : 'ClassDefinitionPrinters-Tests'
}

{ #category : 'initialization' }
MockWithComplexSlot >> initialize [

	<ignoreUnusedVariables: #( #aComplexSlot )>
	self class initializeSlots: self.
	super initialize
]
