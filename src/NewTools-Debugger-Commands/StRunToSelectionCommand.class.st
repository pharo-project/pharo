"
Request the debugger to run the execution up to the cursor selection
"
Class {
	#name : #StRunToSelectionCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #'as yet unclassified' }
StRunToSelectionCommand class >> defaultDescription [
	^ 'Execute methods up to the text cursor position and return debugger control.'
]

{ #category : #initialization }
StRunToSelectionCommand class >> defaultIconName [
	^ #glamorousRightSide
]

{ #category : #'as yet unclassified' }
StRunToSelectionCommand class >> defaultName [
	<toolbarDebugCommand: 50>
	^ 'Run to'
]

{ #category : #initialization }
StRunToSelectionCommand class >> defaultShortcut [
	^ nil
]

{ #category : #hooks }
StRunToSelectionCommand >> execute [
	self debugger runToSelection
]
