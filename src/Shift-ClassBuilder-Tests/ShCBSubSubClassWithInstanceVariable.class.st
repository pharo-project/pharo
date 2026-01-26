"
I am a test class for the ShiftClassBuilder
"
Class {
	#name : 'ShCBSubSubClassWithInstanceVariable',
	#superclass : 'ShCBSubClassWithInstanceVariable',
	#instVars : [
		'anotherSlot'
	],
	#category : 'Shift-ClassBuilder-Tests-TestClasses',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'TestClasses'
}

{ #category : 'initialization' }
ShCBSubSubClassWithInstanceVariable >> initialize [ 

	super initialize.
	anotherSlot := 1.
]
