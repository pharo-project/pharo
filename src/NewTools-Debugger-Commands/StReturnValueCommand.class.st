"
Requests the debugger to return from the current context with the result of the evaluation of a user-defined expression.
"
Class {
	#name : #StReturnValueCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StReturnValueCommand class >> defaultDescription [
	^ 'Return of a given value to the previous context.'
]

{ #category : #initialization }
StReturnValueCommand class >> defaultIconName [

	^ #debuggerReturn
]

{ #category : #accessing }
StReturnValueCommand class >> defaultLabel [

	^ 'Return entered value'
]

{ #category : #accessing }
StReturnValueCommand class >> defaultName [
	<toolbarDebugCommand: 910>
	^ 'Return'
]

{ #category : #initialization }
StReturnValueCommand class >> defaultShortcut [
	^ nil
]

{ #category : #accessing }
StReturnValueCommand >> canBeExecuted [
	"I can always return from a context, for ex. if I'm stuck because of a DNU."
	^ true
]

{ #category : #actions }
StReturnValueCommand >> execute [
	self debugger returnEnteredValue
]
