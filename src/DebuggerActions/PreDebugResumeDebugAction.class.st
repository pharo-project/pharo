"
A PreDebugResumeDebugAction adds the ResumeDebugAction to the pre debug window.

Instance Variables
	preDebugWindow:		<Object>

preDebugWindow
	- xxxxx

"
Class {
	#name : #PreDebugResumeDebugAction,
	#superclass : #ResumeDebugAction,
	#instVars : [
		'preDebugWindow'
	],
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
PreDebugResumeDebugAction class >> actionType [
	<preDebuggingAction>
]

{ #category : #actions }
PreDebugResumeDebugAction >> executeAction [

	self session 
		resume;
		clear.
	self preDebugWindow close. 
]

{ #category : #accessing }
PreDebugResumeDebugAction >> id [

	^ #preDebugResume
]

{ #category : #initialization }
PreDebugResumeDebugAction >> initialize [

	super initialize.
	self needsUpdate: false.
	self needsValidation: false
]

{ #category : #accessing }
PreDebugResumeDebugAction >> preDebugWindow [
	
	^ preDebugWindow
]

{ #category : #accessing }
PreDebugResumeDebugAction >> preDebugWindow: anObject [
	
	preDebugWindow := anObject
]
