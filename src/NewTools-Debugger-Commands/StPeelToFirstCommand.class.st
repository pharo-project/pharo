"
Requests the debugger to peel the stack to the first occurrence of the currently selected message in the stack
"
Class {
	#name : #StPeelToFirstCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StPeelToFirstCommand class >> defaultDescription [
	^ 'Peels the stack back to the second occurance of the currently selected message.'
]

{ #category : #initialization }
StPeelToFirstCommand class >> defaultIconName [
	^ #down
]

{ #category : #accessing }
StPeelToFirstCommand class >> defaultName [
	^ 'Peel to first like this'
]

{ #category : #initialization }
StPeelToFirstCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StPeelToFirstCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #actions }
StPeelToFirstCommand >> execute [
	self debugger copyStackToClipboard
]
