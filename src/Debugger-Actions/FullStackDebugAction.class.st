"
A FullStackDebugAction displays the entire stack in the stack widget.

"
Class {
	#name : #FullStackDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
FullStackDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
FullStackDebugAction >> defaultLabel [

	^ 'Full Stack'
]

{ #category : #accessing }
FullStackDebugAction >> defaultOrder [

	^ 30
]

{ #category : #accessing }
FullStackDebugAction >> enabled [

	^ self shouldEnableFullStackButton
]

{ #category : #actions }
FullStackDebugAction >> executeAction [
	
	self debugger stack expandBy: self expandedStackSize
	
]

{ #category : #accessing }
FullStackDebugAction >> expandedStackSize [

	^ 100000
]

{ #category : #accessing }
FullStackDebugAction >> help [
	^ 'Open a full debugger to navigate in the full bugged stack.'
]

{ #category : #accessing }
FullStackDebugAction >> id [

	^ #fullStack
]

{ #category : #testing }
FullStackDebugAction >> shouldEnableFullStackButton [

	self debugger ifNil: [ ^ false ].  
		
	^ self debugger stack listItems 
		ifEmpty: [ false ] 
		ifNotEmpty: [ :aList |
			aList last sender notNil ] 
]
