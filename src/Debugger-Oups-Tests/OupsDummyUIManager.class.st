"
A dummy UIManager used to test DebuggerSystem
"
Class {
	#name : #OupsDummyUIManager,
	#superclass : #Object,
	#instVars : [
		'callsToHandleDebugRequest',
		'callsToHandleWarningDebugRequest',
		'debugRequestSentToHandleDebugRequest',
		'debugRequestSentToHandleWarningDebugRequest'
	],
	#category : #'Debugger-Oups-Tests-Utils'
}

{ #category : #'handle debug requests' }
OupsDummyUIManager >> callsToHandleWarningDebugRequest [
	callsToHandleWarningDebugRequest ifNil: [ callsToHandleWarningDebugRequest := 0 ].
	^ callsToHandleWarningDebugRequest
]

{ #category : #'handle debug requests' }
OupsDummyUIManager >> callsToHandleWarningDebugRequest: aValue [
	callsToHandleWarningDebugRequest := aValue
]

{ #category : #'handle debug requests' }
OupsDummyUIManager >> debugRequestSentToHandleDebugRequest [
	^ debugRequestSentToHandleDebugRequest
]

{ #category : #'handle debug requests' }
OupsDummyUIManager >> debugRequestSentToHandleWarningDebugRequest [
	^ debugRequestSentToHandleWarningDebugRequest
]

{ #category : #'handle debug requests' }
OupsDummyUIManager >> handleDebugRequest: aDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [
	<debuggerCompleteToSender>
	anOupsDebuggerSystem openDebuggerOnRequest: aDebugRequest
]

{ #category : #'handle debug requests' }
OupsDummyUIManager >> handleWarningDebugRequest: aDebugRequest [
	"Increment a counter to measure how many times this method was called"
	self callsToHandleWarningDebugRequest: self callsToHandleWarningDebugRequest + 1.
	debugRequestSentToHandleWarningDebugRequest := aDebugRequest
]
