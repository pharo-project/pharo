"
A PreDebugSubclassResponsabilityAction adds the SubclassResponsabilityDebugAction debugging actions to the pre debug window. When executed, it will further close the pre debug window and open the full debugger. 

Instance Variables
	preDebugWindow:		<Object>

preDebugWindow
	- xxxxx

"
Class {
	#name : #PreDebugSubclassResponsabilityAction,
	#superclass : #SubclassResponsabilityDebugAction,
	#instVars : [
		'preDebugWindow'
	],
	#category : 'DebuggerActions'
}

{ #category : #registration }
PreDebugSubclassResponsabilityAction class >> actionType [
	<preDebuggingAction>
]

{ #category : #actions }
PreDebugSubclassResponsabilityAction >> executeAction [
	
	super executeAction.
	self preDebugWindow openFullDebugger "Open the full debugger"
]

{ #category : #accessing }
PreDebugSubclassResponsabilityAction >> id [

	^ #preDebugSubclassResponsability
]

{ #category : #initialization }
PreDebugSubclassResponsabilityAction >> initialize [

	super initialize.
	self needsUpdate: false.
	self needsValidation: false
]

{ #category : #accessing }
PreDebugSubclassResponsabilityAction >> preDebugWindow [
	
	^ preDebugWindow
]

{ #category : #accessing }
PreDebugSubclassResponsabilityAction >> preDebugWindow: anObject [
	
	preDebugWindow := anObject
]
