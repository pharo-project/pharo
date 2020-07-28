"
I request the debugger to define a missing class in case the current context is interrupted on a DNU exception and a receiver that is an unknown symbol
"
Class {
	#name : #StDefineClassCommand,
	#superclass : #StDefineMissingEntityCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StDefineClassCommand class >> defaultName [
	^ 'Create missing class'
]

{ #category : #testing }
StDefineClassCommand >> appliesTo: aDebugger [
	^ aDebugger canExecuteCreateMissingClassCommand
]

{ #category : #executing }
StDefineClassCommand >> execute [	
	self debugger createMissingClass
]
