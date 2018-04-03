"
This action adds the possibility of opening a GTDebugger from the PreDebugWindow.
"
Class {
	#name : #GTTemporaryPreDebugChangeDebuggerAction,
	#superclass : #DebugAction,
	#instVars : [
		'preDebugWindow'
	],
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTTemporaryPreDebugChangeDebuggerAction class >> actionType [
	<preDebuggingAction>
]

{ #category : #testing }
GTTemporaryPreDebugChangeDebuggerAction >> appliesToDebugger: aDebugger [

	^ (aDebugger class inheritsFrom: GTMoldableDebugger) not 
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> defaultLabel [

	^ 'Debug Glamorously'
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> defaultOrder [

	^ 200
]

{ #category : #actions }
GTTemporaryPreDebugChangeDebuggerAction >> executeAction [
	| currentDebugger currentSession |
	
	"Close the spec PreDebugWindow and debugger"
	currentDebugger := self preDebugWindow debugger.
	self preDebugWindow debugger: nil.
	self preDebugWindow close.
	currentSession := currentDebugger session.
	currentDebugger session: nil.
	currentDebugger clear. "clear not close, as at this point there is no open window to close"
	
	GTGenericStackDebugger debugSession: currentSession
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> help [
	^ 'Open a GTDebugger from the PreDebugWindow.'
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> id [

	^ #gtChangeDebuggerPreDebug
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> preDebugWindow [
	
	^ preDebugWindow
]

{ #category : #accessing }
GTTemporaryPreDebugChangeDebuggerAction >> preDebugWindow: anObject [
	
	preDebugWindow := anObject
]
