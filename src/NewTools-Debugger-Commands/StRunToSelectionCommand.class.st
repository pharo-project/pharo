"
Request the debugger to run the execution up to the cursor selection
"
Class {
	#name : #StRunToSelectionCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StRunToSelectionCommand class >> defaultDescription [
	^ 'Execute methods up to the text cursor position and return debugger control.'
]

{ #category : #default }
StRunToSelectionCommand class >> defaultIconName [

	^ #debuggerRunToSelection
]

{ #category : #default }
StRunToSelectionCommand class >> defaultName [
	<toolbarDebugCommand: 50>
	^ 'Run to'
]

{ #category : #default }
StRunToSelectionCommand class >> defaultShortcut [
	^ nil
]

{ #category : #hooks }
StRunToSelectionCommand >> execute [
	self debugger runToSelection
]
