"
Requests the debugger to step over the next expression evaluation
"
Class {
	#name : #StStepThroughCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StStepThroughCommand class >> defaultDescription [
	^ 'Step over the highlighted message, i.e. do not follow the sent message, taking debugger control after the method invoked returns or whenever execution should return inside a block used as an argument before that.'
]

{ #category : #initialization }
StStepThroughCommand class >> defaultIconName [

	^ #debuggerStepThrough 
]

{ #category : #default }
StStepThroughCommand class >> defaultName [
	<toolbarDebugCommand: 30>

	^ 'Through'
]

{ #category : #initialization }
StStepThroughCommand class >> defaultShortcut [
	^ PharoShortcuts current stepThroughShortcut
]

{ #category : #hooks }
StStepThroughCommand >> execute [
	self debugger stepThrough
]
