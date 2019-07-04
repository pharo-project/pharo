"
A WhereIsDebugAction highlights in the code editor the current instruction.

"
Class {
	#name : #WhereIsDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
WhereIsDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
WhereIsDebugAction >> defaultLabel [

	^ 'Where is?'
]

{ #category : #accessing }
WhereIsDebugAction >> defaultOrder [

	^ 40
]

{ #category : #actions }
WhereIsDebugAction >> executeAction [

	self debugger code setSelection: (self session 
		pcRangeForContext:  self currentContext)	
]

{ #category : #accessing }
WhereIsDebugAction >> help [
	^ 'Highligh the next method to be executed (step) by the debugger.'
]

{ #category : #accessing }
WhereIsDebugAction >> id [

	^ #whereIs
]

{ #category : #initialization }
WhereIsDebugAction >> initialize [

	super initialize.
	"This action just updates the selection in the code editor. 
	No need to do any update or validation."
	self needsUpdate: false.
	self needsValidation: false.
	self withSeparatorAfter
]
