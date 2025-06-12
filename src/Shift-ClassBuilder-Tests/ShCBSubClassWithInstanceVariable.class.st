"
I am a test class for the ShiftClassBuilder
"
Class {
	#name : 'ShCBSubClassWithInstanceVariable',
	#superclass : 'ShCBClassWithSubclass',
	#instVars : [
		'aSlot'
	],
	#category : 'Shift-ClassBuilder-Tests-TestClasses',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'TestClasses'
}

{ #category : 'initialization' }
ShCBSubClassWithInstanceVariable >> initialize [ 

	super initialize.
	aSlot := 1.
]
