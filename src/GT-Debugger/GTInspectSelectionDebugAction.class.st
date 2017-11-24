"
This action is responsible for inspecting a selected piece of code from the code pane of the debugger.
"
Class {
	#name : #GTInspectSelectionDebugAction,
	#superclass : #GTCodeDebugAction,
	#instVars : [
		'shouldBasicInspect'
	],
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTInspectSelectionDebugAction class >> gtActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousInspect
]

{ #category : #registration }
GTInspectSelectionDebugAction class >> gtBasicInspectActionFor: aDebugger [
	"<gtContextCodeDebuggingAction>"
	
	^ (self forDebugger: aDebugger)
			makeBasicInspect;
			label: 'Basic inspect it';
			keymap: $i meta shift; 
			order: 16;
			icon: GLMUIThemeExtraIcons glamorousInspect
]

{ #category : #accessing }
GTInspectSelectionDebugAction >> defaultKeymap [

	^ PharoShortcuts current inspectItShortcut 
]

{ #category : #accessing }
GTInspectSelectionDebugAction >> defaultLabel [

	^ 'Inspect it'
]

{ #category : #accessing }
GTInspectSelectionDebugAction >> defaultOrder [

	^ 15
]

{ #category : #actions }
GTInspectSelectionDebugAction >> executeAction [
	
	self codePresentation evaluateSelectionAndDo: [ :result |
		shouldBasicInspect 
			ifTrue: [ result basicInspect ]
			ifFalse: [ result inspect ] ]
		

]

{ #category : #accessing }
GTInspectSelectionDebugAction >> id [

	^ #gTInspectSelectionDebugAction
]

{ #category : #initialization }
GTInspectSelectionDebugAction >> initialize [
	super initialize.
	self needsUpdate: false.
	shouldBasicInspect := false
]

{ #category : #actions }
GTInspectSelectionDebugAction >> makeBasicInspect [

	shouldBasicInspect := true
]
