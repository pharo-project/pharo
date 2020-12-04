"
I request the debugger to define a missing method in case the current context is interrupted on a DNU exception
"
Class {
	#name : #StDefineMethodCommand,
	#superclass : #StDefineMissingEntityCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StDefineMethodCommand class >> defaultName [
	^ 'Create missing command'
]

{ #category : #testing }
StDefineMethodCommand >> appliesTo: aDebugger [
	^ aDebugger canExecuteCreateMissingMethodCommand
]

{ #category : #hooks }
StDefineMethodCommand >> execute [
	self debugger createMissingMethod
]
