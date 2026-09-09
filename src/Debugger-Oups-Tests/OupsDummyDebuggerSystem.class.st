"
I am a dummy counterpart to the DebuggerSystem class, used to test it.
"
Class {
	#name : 'OupsDummyDebuggerSystem',
	#superclass : 'OupsDebuggerSystem',
	#instVars : [
		'debugRequestSentToHandleDebugRequest',
		'callsToHandleDebugRequest'
	],
	#category : 'Debugger-Oups-Tests-Utils',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Utils'
}

{ #category : 'accessing' }
OupsDummyDebuggerSystem >> callsToHandleDebugRequest [
	^callsToHandleDebugRequest ifNil: [ callsToHandleDebugRequest := 0 ]
]

{ #category : 'accessing' }
OupsDummyDebuggerSystem >> callsToHandleDebugRequest: aValue [
	callsToHandleDebugRequest := aValue
]

{ #category : 'accessing' }
OupsDummyDebuggerSystem >> debugRequestSentToHandleDebugRequest [
	^ debugRequestSentToHandleDebugRequest
]

{ #category : 'default values' }
OupsDummyDebuggerSystem >> debuggerSelectionStrategy [
	^ OupsDummySelectionStrategy new
]

{ #category : 'open debugger' }
OupsDummyDebuggerSystem >> openDebuggerOnRequest: aDebugRequest [
	self callsToHandleDebugRequest: self callsToHandleDebugRequest + 1.
	debugRequestSentToHandleDebugRequest := aDebugRequest.
	self performPreDebugActionsIn: aDebugRequest
]

{ #category : 'helpers' }
OupsDummyDebuggerSystem >> suspendDebuggedProcess: aDebugRequest [
	"Do not actually suspend the debugged process, in order to prevent the UI process from being suspended (image freeze) if a test goes wrong"
	^ 1
]

{ #category : 'open debugger' }
OupsDummyDebuggerSystem >> warningRequest: aDebugRequest withErrorHandler: errorHandler [

	self openDebuggerOnRequest: aDebugRequest
]
