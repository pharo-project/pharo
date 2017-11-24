"
This action is responsible for evaluating the selection in a code pane of a debugger and pushing the result in the inspector of the debugger.
"
Class {
	#name : #GTSelectionGoDebugAction,
	#superclass : #GTCodeDebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTSelectionGoDebugAction class >> gtActionFor: aDebugger [
	<gtContextCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
]

{ #category : #accessing }
GTSelectionGoDebugAction >> defaultKeymap [

	^ $g meta
]

{ #category : #accessing }
GTSelectionGoDebugAction >> defaultLabel [

	^ 'Do it and go'
]

{ #category : #accessing }
GTSelectionGoDebugAction >> defaultOrder [

	^ 5
]

{ #category : #actions }
GTSelectionGoDebugAction >> executeAction [
	
	self codePresentation 
		evaluateSelectionAndDo: [ :result | self debugger inspect: result ] 
		

]

{ #category : #accessing }
GTSelectionGoDebugAction >> id [

	^ #gTOpenSelectionDebugAction
]

{ #category : #initialization }
GTSelectionGoDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
