"
This action encapsulates the logic of debugging a selected piece of code from the code pane of the debugger.
"
Class {
	#name : #GTDebugSelectionDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTDebugSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
]

{ #category : #accessing }
GTDebugSelectionDebugAction >> defaultKeymap [

	^ PharoShortcuts current debugItShortcut
]

{ #category : #accessing }
GTDebugSelectionDebugAction >> defaultLabel [

	^ 'Debug it'
]

{ #category : #accessing }
GTDebugSelectionDebugAction >> defaultOrder [

	^ 20
]

{ #category : #actions }
GTDebugSelectionDebugAction >> executeAction [
	
	self codePresentation debugSelection
		

]

{ #category : #accessing }
GTDebugSelectionDebugAction >> help [
	^ 'Debug a selected piece of code from the code pane of the debugger.'
]

{ #category : #accessing }
GTDebugSelectionDebugAction >> id [

	^ #gTDebugSelectionDebugAction
]

{ #category : #initialization }
GTDebugSelectionDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
