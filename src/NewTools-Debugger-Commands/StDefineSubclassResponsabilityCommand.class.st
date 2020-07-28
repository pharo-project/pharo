"
I request the debugger to define a subclass responsibility method, in case the current context is interrupted on a subclass responsibility exception
"
Class {
	#name : #StDefineSubclassResponsabilityCommand,
	#superclass : #StDefineMissingEntityCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StDefineSubclassResponsabilityCommand class >> defaultDescription [
	^ 'Create a method in a class having the responsability to implement it.'
]

{ #category : #initialization }
StDefineSubclassResponsabilityCommand class >> defaultIconName [
	^ #changeAdd
]

{ #category : #accessing }
StDefineSubclassResponsabilityCommand class >> defaultName [
	^ 'Create subclass responsibility'
]

{ #category : #initialization }
StDefineSubclassResponsabilityCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StDefineSubclassResponsabilityCommand >> appliesTo: aDebugger [
	^ aDebugger canExecuteCreateSubclassResponsibilityCommand
]

{ #category : #actions }
StDefineSubclassResponsabilityCommand >> execute [
	self debugger createSubclassResponsibility
]
