"
I am a debugger that will log the error in an `#inform:` when called.
"
Class {
	#name : #InformDebugger,
	#superclass : #LogDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'opening api' }
InformDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self inform: (self messageToPrintFrom: aDebugSession)
]
