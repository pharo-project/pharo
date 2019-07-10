"
A RunToSelectionDebugAction resumes the execution until the selected position in the source code is reached.
"
Class {
	#name : #RunToSelectionDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
RunToSelectionDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuCodeDebuggingAction>
]

{ #category : #accessing }
RunToSelectionDebugAction >> defaultLabel [

	^ 'Run to here'
]

{ #category : #accessing }
RunToSelectionDebugAction >> defaultOrder [

	^ 35
]

{ #category : #actions }
RunToSelectionDebugAction >> executeAction [

	self session 
		runToSelection: self debugger code getSelection 
		inContext: self currentContext
]

{ #category : #accessing }
RunToSelectionDebugAction >> help [
	^ 'Execute methods up to the text cursor position and return debugger control.'
]

{ #category : #accessing }
RunToSelectionDebugAction >> id [

	^ #runToHere
]
