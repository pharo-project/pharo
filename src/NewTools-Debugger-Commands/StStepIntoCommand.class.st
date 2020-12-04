"
Requests the debugger to step into the next expression evaluation
"
Class {
	#name : #StStepIntoCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StStepIntoCommand class >> defaultDescription [
	^ 'Step in the highlighted message, i.e. follow the sent message, taking debugger control in the method invoked.'
]

{ #category : #initialization }
StStepIntoCommand class >> defaultIconName [

	^ #debuggerStepInto
]

{ #category : #default }
StStepIntoCommand class >> defaultName [
	<toolbarDebugCommand: 20>
	
	^ 'Into'
]

{ #category : #initialization }
StStepIntoCommand class >> defaultShortcut [
	^ PharoShortcuts current stepIntoShortcut
]

{ #category : #hooks }
StStepIntoCommand >> execute [
	self debugger stepInto
]
