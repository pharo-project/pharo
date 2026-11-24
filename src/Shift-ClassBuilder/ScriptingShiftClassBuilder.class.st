"
I'm a little extension to support the fact that class can be automatically installed using a different message `<<<` instead of `<<` for the class definition.
"
Class {
	#name : 'ScriptingShiftClassBuilder',
	#superclass : 'ShiftClassBuilder',
	#category : 'Shift-ClassBuilder-Builder',
	#package : 'Shift-ClassBuilder',
	#tag : 'Builder'
}

{ #category : 'accessing' }
ScriptingShiftClassBuilder >> package: aString [

	super package: aString. 
	self install.
	"does not return the class for now. This is a point to be discuss."
]
