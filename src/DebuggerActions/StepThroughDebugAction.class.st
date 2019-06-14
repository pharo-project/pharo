"
A StepThroughDebugAction through the execution until control returns to the current method.

"
Class {
	#name : #StepThroughDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
StepThroughDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
StepThroughDebugAction >> defaultKeymap [

	^ PharoShortcuts current stepThroughShortcut
]

{ #category : #accessing }
StepThroughDebugAction >> defaultLabel [

	^ 'Through'
]

{ #category : #accessing }
StepThroughDebugAction >> defaultOrder [

	^ 25
]

{ #category : #actions }
StepThroughDebugAction >> executeAction [

	self session stepThrough: self currentContext
]

{ #category : #accessing }
StepThroughDebugAction >> help [
	^ 'Step over the highlighted message, i.e. do not follow the sent message, taking debugger control after the method invoked returns or whenever execution should return inside a block used as an argument before that.'
]

{ #category : #accessing }
StepThroughDebugAction >> id [

	^ #stepThrough
]
