"
Requests the debugger to step over the next expression evaluation
"
Class {
	#name : #StStepOverCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StStepOverCommand class >> defaultDescription [
	^'Step over the highlighted message, i.e. do not follow the sent message, taking debugger control after the method invoked returns.'
]

{ #category : #initialization }
StStepOverCommand class >> defaultIconName [

	^ #debuggerStepOver
]

{ #category : #accessing }
StStepOverCommand class >> defaultName [
	<toolbarDebugCommand: 40>

	^ 'Over'
]

{ #category : #initialization }
StStepOverCommand class >> defaultShortcut [
	^ PharoShortcuts current stepOverShortcut
]

{ #category : #hooks }
StStepOverCommand >> execute [
	self debugger stepOver
]
