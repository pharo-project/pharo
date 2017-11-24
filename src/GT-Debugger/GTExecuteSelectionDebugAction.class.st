"
This action is responsible for executing a selected piece of code from the code pane of the debugger.
"
Class {
	#name : #GTExecuteSelectionDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTExecuteSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousSpawn
]

{ #category : #accessing }
GTExecuteSelectionDebugAction >> defaultKeymap [

	^ PharoShortcuts current doItShortcut
]

{ #category : #accessing }
GTExecuteSelectionDebugAction >> defaultLabel [

	^ 'Do it'
]

{ #category : #accessing }
GTExecuteSelectionDebugAction >> defaultOrder [

	^ 10
]

{ #category : #actions }
GTExecuteSelectionDebugAction >> executeAction [
	
	self codePresentation evaluateSelection
		

]

{ #category : #accessing }
GTExecuteSelectionDebugAction >> id [

	^ #gTExecuteSelectionDebugAction
]

{ #category : #initialization }
GTExecuteSelectionDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
