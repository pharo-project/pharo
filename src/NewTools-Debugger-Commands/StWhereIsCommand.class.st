"
Requests the debugger to reselect in its code pane the next expression to be executed
"
Class {
	#name : #StWhereIsCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #default }
StWhereIsCommand class >> defaultDescription [
	^ 'Highligh the next method to be executed (step) by the debugger.'
]

{ #category : #initialization }
StWhereIsCommand class >> defaultIconName [

	^ #debuggerWhereIs 
]

{ #category : #default }
StWhereIsCommand class >> defaultName [
	<toolbarDebugCommand: 1000>
	^ 'Where is?'
]

{ #category : #initialization }
StWhereIsCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StWhereIsCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #hooks }
StWhereIsCommand >> execute [
	self debugger selectNextExecutedExpression
]
