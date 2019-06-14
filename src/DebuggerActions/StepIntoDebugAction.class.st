"
A StepIntoDebugAction the current context.

"
Class {
	#name : #StepIntoDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
StepIntoDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
StepIntoDebugAction >> defaultKeymap [

	^ PharoShortcuts current stepIntoShortcut
]

{ #category : #accessing }
StepIntoDebugAction >> defaultLabel [

	^ 'Into'
]

{ #category : #accessing }
StepIntoDebugAction >> defaultOrder [

	^ 15
]

{ #category : #actions }
StepIntoDebugAction >> executeAction [

	self session stepInto: self currentContext
]

{ #category : #accessing }
StepIntoDebugAction >> help [
	^ 'Step in the highlighted message, i.e. follow the sent message, taking debugger control in the method invoked.'
]

{ #category : #accessing }
StepIntoDebugAction >> id [

	^ #stepInto
]
