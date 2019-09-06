"
I am a debugger that will log the error in a Trascript when called.
"
Class {
	#name : #TranscriptDebugger,
	#superclass : #LogDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'opening api' }
TranscriptDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self traceCr: (self messageToPrintFrom: aDebugSession)
]
