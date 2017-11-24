"
This action is responsible for canceling/clearing the changes in a code editor from a debugger.
"
Class {
	#name : #GTClearEditorDebugAction,
	#superclass : #DebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTClearEditorDebugAction class >> gtActionFor: aDebugger [
	<gtContextCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
		icon: GLMUIThemeExtraIcons glamorousCancel
]

{ #category : #accessing }
GTClearEditorDebugAction >> defaultIcon [

	^ nil
	"^ Smalltalk ui icons smallCancelIcon"
]

{ #category : #accessing }
GTClearEditorDebugAction >> defaultKeymap [

	^ PharoShortcuts current cancelShortcut
]

{ #category : #accessing }
GTClearEditorDebugAction >> defaultLabel [

	^ 'Cancel'
]

{ #category : #accessing }
GTClearEditorDebugAction >> defaultOrder [

	^ 260
]

{ #category : #actions }
GTClearEditorDebugAction >> executeAction [

	self debugger codePresentation hasUnacceptedEdits ifTrue: [  
		self debugger codePresentation clearUserEdits.
		self debugger updateCodeEditor ]
]

{ #category : #accessing }
GTClearEditorDebugAction >> id [

	^ #gtClearEditorDebugAction
]

{ #category : #initialization }
GTClearEditorDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
