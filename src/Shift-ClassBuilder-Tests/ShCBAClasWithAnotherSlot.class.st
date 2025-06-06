"
I am a test class for the ShiftClassBuilder
"
Class {
	#name : 'ShCBAClasWithAnotherSlot',
	#superclass : 'Object',
	#instVars : [
		'anotherSlot'
	],
	#category : 'Shift-ClassBuilder-Tests-TestClasses',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'TestClasses'
}

{ #category : 'initialization' }
ShCBAClasWithAnotherSlot >> initialize [ 

	super initialize.
	anotherSlot := 1.
]
