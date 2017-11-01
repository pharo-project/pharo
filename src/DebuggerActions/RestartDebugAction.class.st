"
A RestartDebugAction restarts the execution of the current context.

"
Class {
	#name : #RestartDebugAction,
	#superclass : #DebugAction,
	#category : 'DebuggerActions'
}

{ #category : #registration }
RestartDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
RestartDebugAction >> defaultHelp [
	^ 'Go back the start of the current execution context, resetting all local variables and take debugger control.'
]

{ #category : #accessing }
RestartDebugAction >> defaultKeymap [

	^ PharoShortcuts current restartExecutionShortcut
]

{ #category : #accessing }
RestartDebugAction >> defaultLabel [

	^ 'Restart'
]

{ #category : #accessing }
RestartDebugAction >> defaultOrder [

	^ 10
]

{ #category : #actions }
RestartDebugAction >> executeAction [

	self session restart: self currentContext
]

{ #category : #accessing }
RestartDebugAction >> id [

	^ #restart
]
