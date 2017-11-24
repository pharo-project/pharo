"
This action is responsible for profiling  a selected piece of code from the code pane of the debugger.
"
Class {
	#name : #GTProfileSelectionDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTProfileSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
]

{ #category : #accessing }
GTProfileSelectionDebugAction >> defaultLabel [

	^ 'Profile it'
]

{ #category : #accessing }
GTProfileSelectionDebugAction >> defaultOrder [

	^ 30
]

{ #category : #actions }
GTProfileSelectionDebugAction >> executeAction [
	
	self codePresentation profileSelection
		

]

{ #category : #accessing }
GTProfileSelectionDebugAction >> id [

	^ #gTProfileSelectionDebugAction
]

{ #category : #initialization }
GTProfileSelectionDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
