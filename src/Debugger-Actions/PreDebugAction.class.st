"
A PreDebugAction is a debugging actions sending a unary message to the pre debug window.

"
Class {
	#name : #PreDebugAction,
	#superclass : #MessageSendDebugAction,
	#instVars : [
		'preDebugWindow'
	],
	#category : #'Debugger-Actions-Actions'
}

{ #category : #'actions creation' }
PreDebugAction class >> debugActionsFor: aDebugger [
	<preDebuggingAction>
	
	^ { 
		self new
			id: #abandonAction;
			selector: #close;
			order: 10;
			help: 'Close the debugger and ignore the exception';
			label: 'Abandon'.
		self new
			id: #openFullDebuggerAction;
			selector: #openFullDebugger;
			order: 15;
			help: 'Open a full debugger to navigate in the full bugged stack.';
			label: 'Debug'
	}
]

{ #category : #initialization }
PreDebugAction >> initialize [

	super initialize.
	self needsUpdate: false.
	self needsValidation: false.
]

{ #category : #accessing }
PreDebugAction >> preDebugWindow [
	
	^ preDebugWindow
]

{ #category : #accessing }
PreDebugAction >> preDebugWindow: anObject [
	
	preDebugWindow := anObject
]

{ #category : #accessing }
PreDebugAction >> receiver [

	^ self preDebugWindow
]
