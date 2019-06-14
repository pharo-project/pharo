Class {
	#name : #InspectDebuggerDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
InspectDebuggerDebugAction class >> actionType [
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
InspectDebuggerDebugAction >> defaultLabel [
	^ 'Inspect debugger'
]

{ #category : #accessing }
InspectDebuggerDebugAction >> defaultOrder [
	^ 130
]

{ #category : #accessing }
InspectDebuggerDebugAction >> executeAction [ 
	self debugger inspect.
]

{ #category : #accessing }
InspectDebuggerDebugAction >> help [
	^ 'Opens an inspector on the underlying debugger object'
]

{ #category : #accessing }
InspectDebuggerDebugAction >> id [
	^ #inspectDebugger
]
