"
Request the debugger to restart its current context
"
Class {
	#name : #StRestartCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StRestartCommand class >> defaultDescription [
	^ 'Go back the start of the current execution context, resetting all local variables and take debugger control.'
]

{ #category : #initialization }
StRestartCommand class >> defaultIconName [

	^ #debuggerRestart
]

{ #category : #default }
StRestartCommand class >> defaultName [
	<toolbarDebugCommand: 900>

	^ 'Restart'
]

{ #category : #initialization }
StRestartCommand class >> defaultShortcut [
	^ PharoShortcuts current restartExecutionShortcut
]

{ #category : #testing }
StRestartCommand >> appliesTo: aDebugger [
	^ aDebugger canExecuteRestartCommand
]

{ #category : #hooks }
StRestartCommand >> execute [
	self debugger restartCurrentContext
]
