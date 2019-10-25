"
I am a debugger that will log the error in a Trascript when called.
"
Class {
	#name : #TranscriptDebugger,
	#superclass : #LogDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #testing }
TranscriptDebugger class >> handlesContext: aContext [
	"Returns whether this debugger is suitable to debug @aContext"
	^ true
]

{ #category : #'opening api' }
TranscriptDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	<iAmADebugger>
]

{ #category : #'opening api' }
TranscriptDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self traceCr: (self messageToPrintFrom: aDebugSession)
]

{ #category : #'as yet unclassified' }
TranscriptDebugger class >> rank [
	"Returns an int. When an exception needs to  be debugged, the rank is used to choose among the available debuggers. A higher rank means a higher priority."
	^10
]
