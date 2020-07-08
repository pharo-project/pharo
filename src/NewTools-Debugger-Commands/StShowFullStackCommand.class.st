"
Request the debugger to show all the stack in its stack pane
"
Class {
	#name : #StShowFullStackCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StShowFullStackCommand class >> defaultDescription [
	^ 'Shows the full stack.'
]

{ #category : #initialization }
StShowFullStackCommand class >> defaultIconName [
	^ #glamorousMore
]

{ #category : #default }
StShowFullStackCommand class >> defaultName [
	<toolbarDebugCommand: 999>
	^ 'Stack'
]

{ #category : #initialization }
StShowFullStackCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StShowFullStackCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #hooks }
StShowFullStackCommand >> execute [
	self debugger showFullStack
]
