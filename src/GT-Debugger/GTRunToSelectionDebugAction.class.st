"
This action is responsible for continuing the execution up to the point selected in the code pane.
"
Class {
	#name : #GTRunToSelectionDebugAction,
	#superclass : #RunToSelectionDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTRunToSelectionDebugAction class >> gtActionFor: aDebugger [
	<gtContextCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousPlay
]

{ #category : #accessing }
GTRunToSelectionDebugAction >> defaultOrder [

	^ 1
]

{ #category : #actions }
GTRunToSelectionDebugAction >> executeAction [

	self session 
		runToSelection: self debugger selectionInterval
		inContext: self currentContext
]

{ #category : #accessing }
GTRunToSelectionDebugAction >> id [

	^ #gtRunToSelection
]
