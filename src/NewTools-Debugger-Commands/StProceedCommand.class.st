"
Request the debugger to proceed the debug session
"
Class {
	#name : #StProceedCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StProceedCommand class >> defaultDescription [
	^ 'Relinquish debugger control and proceed execution from the current point of debugger control.'
]

{ #category : #initialization }
StProceedCommand class >> defaultIconName [
	^ #debuggerProceed
]

{ #category : #default }
StProceedCommand class >> defaultName [
	<toolbarDebugCommand: 10>
	^ 'Proceed'
]

{ #category : #initialization }
StProceedCommand class >> defaultShortcut [
	^ PharoShortcuts current resumeExecutionShortcut
]

{ #category : #hooks }
StProceedCommand >> execute [
	self debugger proceedDebugSession
]
