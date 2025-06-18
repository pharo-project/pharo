"
I am a test class for the ShiftClassBuilder
"
Class {
	#name : 'ShCBAClasWithAnSlot',
	#superclass : 'Object',
	#instVars : [
		'aSlot'
	],
	#category : 'Shift-ClassBuilder-Tests-TestClasses',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'TestClasses'
}

{ #category : 'initialization' }
ShCBAClasWithAnSlot >> initialize [ 

	super initialize.
	aSlot := 1.
]
