"
I am a debugging action that inspect the currently selected bytecode.

"
Class {
	#name : #GTGoAndInspectBytecodeDebugAction,
	#superclass : #DebugAction,
	#category : #'GT-BytecodeDebugger-Actions'
}

{ #category : #registration }
GTGoAndInspectBytecodeDebugAction class >> gtBytecodeDebuggerActionFor: aDebugger [
	<gtContextBytecodeCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
]

{ #category : #accessing }
GTGoAndInspectBytecodeDebugAction >> defaultKeymap [

	^ $g meta
]

{ #category : #accessing }
GTGoAndInspectBytecodeDebugAction >> defaultLabel [

	^ 'Go and inspect'
]

{ #category : #actions }
GTGoAndInspectBytecodeDebugAction >> executeAction [
	
	self debugger inspect: self debugger bytecodePresentation selection  
		

]

{ #category : #accessing }
GTGoAndInspectBytecodeDebugAction >> help [
	^ 'Inspect the currently selected bytecode.'
]

{ #category : #accessing }
GTGoAndInspectBytecodeDebugAction >> id [

	^ GTGoAndInspectBytecodeDebugAction name
]

{ #category : #initialization }
GTGoAndInspectBytecodeDebugAction >> initialize [

	super initialize.
	self needsUpdate: false
]
