"
This action is responsible for printing a selected piece of code from the code pane of the debugger.
"
Class {
	#name : #GTPrintSelectionDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTPrintSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
		
]

{ #category : #accessing }
GTPrintSelectionDebugAction >> defaultKeymap [

	^ PharoShortcuts current printItShortcut
]

{ #category : #accessing }
GTPrintSelectionDebugAction >> defaultLabel [

	^ 'Print it'
]

{ #category : #accessing }
GTPrintSelectionDebugAction >> defaultOrder [

	^ 15
]

{ #category : #actions }
GTPrintSelectionDebugAction >> executeAction [
	
	self codePresentation printSelection
		

]

{ #category : #accessing }
GTPrintSelectionDebugAction >> id [

	^ #gTPrintSelectionDebugAction
]

{ #category : #initialization }
GTPrintSelectionDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
