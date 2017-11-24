"
A GTGenericStackDebuggerSelectionChanged is an event send to the debugger after the selection in the stack pane is changed.
"
Class {
	#name : #GTGenericStackDebuggerSelectionChanged,
	#superclass : #Announcement,
	#instVars : [
		'debugger',
		'newContext'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #'instance creation' }
GTGenericStackDebuggerSelectionChanged class >> forDebugger: aDebugger [

	^ self new forDebugger: aDebugger
]

{ #category : #accessing }
GTGenericStackDebuggerSelectionChanged >> debugger [

	^ debugger
]

{ #category : #initialization }
GTGenericStackDebuggerSelectionChanged >> forDebugger: aDebugger [

	debugger := aDebugger.
	newContext := aDebugger currentContext.
]

{ #category : #accessing }
GTGenericStackDebuggerSelectionChanged >> newContext [

	^ newContext
]
