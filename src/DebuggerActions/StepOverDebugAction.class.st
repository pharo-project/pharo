"
A StepOverDebugAction the current context.

"
Class {
	#name : #StepOverDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
StepOverDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
StepOverDebugAction >> defaultKeymap [

	^ PharoShortcuts current stepOverShortcut
]

{ #category : #accessing }
StepOverDebugAction >> defaultLabel [

	^ 'Over'
]

{ #category : #accessing }
StepOverDebugAction >> defaultOrder [

	^ 20
]

{ #category : #actions }
StepOverDebugAction >> executeAction [

	self session stepOver: self currentContext
]

{ #category : #accessing }
StepOverDebugAction >> help [
	^ 'Step over the highlighted message, i.e. do not follow the sent message, taking debugger control after the method invoked returns.'
]

{ #category : #accessing }
StepOverDebugAction >> id [

	^ #stepOver
]
