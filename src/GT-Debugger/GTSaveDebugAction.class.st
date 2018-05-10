"
This debugger action is responsible for saving the current method from a debugger.
"
Class {
	#name : #GTSaveDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTSaveDebugAction class >> gtActionFor: aDebugger [
	<gtContextCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousAccept
]

{ #category : #accessing }
GTSaveDebugAction >> defaultKeymap [

	^ PharoShortcuts current acceptShortcut
]

{ #category : #initialization }
GTSaveDebugAction >> defaultLabel [

	^ 'Accept'
]

{ #category : #initialization }
GTSaveDebugAction >> defaultOrder [

	^ 250
]

{ #category : #actions }
GTSaveDebugAction >> executeAction [

	self needsUpdate: false.
	self codePresentation hasUnacceptedEdits 
		ifTrue: [
			self codePresentation 
				compileTextIn: self currentContext 
				from: self session 
				andDo: [ self needsUpdate: true ] ].
	self codePresentation flash
]

{ #category : #accessing }
GTSaveDebugAction >> help [
	^ 'Save the current method.'
]

{ #category : #accessing }
GTSaveDebugAction >> id [

	^ #gTSaveDebugAction
]
