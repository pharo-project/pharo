"
I am an action for switching to another debugger.
My class side methods register one such action for every debugger that can be used with the current execution.
"
Class {
	#name : #GTChangeDebuggerAction,
	#superclass : #DebugAction,
	#instVars : [
		'debuggerClass',
		'newDebugger'
	],
	#category : #'GT-Debugger-Actions'
}

{ #category : #accessing }
GTChangeDebuggerAction class >> changeDebuggerActionsForContext: aContext excluding: aDebugger [
	
	^ (self debuggersForContext: aContext excluding: aDebugger) collect: [ :aDebuggerClass | 
		(self forDebugger: aDebugger)
			debuggerClass: aDebuggerClass;
			icon: (Form extent: 0@0) ]
]

{ #category : #accessing }
GTChangeDebuggerAction class >> debuggersForContext: aContext excluding: aDebugger [ 
	"Ungly check to make sure the debugger will work until everything is in the image"
	(Smalltalk tools respondsTo: #debuggersForContext:) 
		ifTrue: [ ^ (Smalltalk tools debuggersForContext: aContext) reverse
			remove: aDebugger class ifAbsent: [ ];
			yourself]
		ifFalse: [ ^ OrderedCollection new ]
]

{ #category : #'as yet unclassified' }
GTChangeDebuggerAction class >> gtChangeDebuggerActionsFor: aDebugger [
	<gtDebuggingAction>
	
	^ self 
		changeDebuggerActionsForContext: aDebugger interruptedContext 
		excluding: aDebugger
]

{ #category : #accessing }
GTChangeDebuggerAction >> debuggerClass: aClass [

	debuggerClass := aClass.
	self label: debuggerClass defaultTitle.
]

{ #category : #initialization }
GTChangeDebuggerAction >> defaultCategory [ 
	
	^ 'Available Debuggers'
]

{ #category : #initialization }
GTChangeDebuggerAction >> defaultOrder [

	^ 100
]

{ #category : #actions }
GTChangeDebuggerAction >> executeAction [

	 newDebugger := self debugger debug: self interruptedContext using: debuggerClass
]

{ #category : #accessing }
GTChangeDebuggerAction >> help [
	^ 'Switch to another debugger'
]

{ #category : #initialization }
GTChangeDebuggerAction >> initialize [
	super initialize.
	self needsUpdate: false
]

{ #category : #accessing }
GTChangeDebuggerAction >> newDebugger [
	^ newDebugger
]
