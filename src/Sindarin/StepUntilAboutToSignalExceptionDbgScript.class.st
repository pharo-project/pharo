"
When executed, this script steps the execution until it is about to signal an exception.
"
Class {
	#name : #StepUntilAboutToSignalExceptionDbgScript,
	#superclass : #SindarinScript,
	#instVars : [
		'scdbg'
	],
	#category : #Sindarin
}

{ #category : #accessing }
StepUntilAboutToSignalExceptionDbgScript class >> scriptName [
	^ 'StepUntilAboutToSignalException'
]

{ #category : #running }
StepUntilAboutToSignalExceptionDbgScript >> contextIsAboutToSignalException: aContext [
	"Returns whether aContext is about to execute a message-send of selector #signal to an instance of the Exception class (or one of its subclasses)"
	| node |
	node := aContext method sourceNodeForPC: aContext pc.
	(node isMessage) ifFalse: [ ^false ].
	(node selector = #signal) ifFalse: [ ^false ].
	(aContext basicSize >= 1) ifFalse: [ ^false ].
	(Exception allSubclasses includes: (aContext at: (aContext basicSize))) ifTrue: [ ^true ]. "#signal sent to a subclass of Exception"
	(Exception allSubclasses includes: ((aContext at: (aContext basicSize)) class)) ifTrue: [ ^true ]. "#signal sent to an instance of a subclass of Exception"
	^ false
	
]

{ #category : #running }
StepUntilAboutToSignalExceptionDbgScript >> execute: aScriptableDebugger [
	scdbg := aScriptableDebugger.
	[ self contextIsAboutToSignalException: scdbg context ] whileFalse: [ scdbg step ].
]

{ #category : #running }
StepUntilAboutToSignalExceptionDbgScript >> execute: aScriptableDebugger withArgs: anOrderedCollection [
	self execute: aScriptableDebugger
]
