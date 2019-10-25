"
I am a debugger that will log the error in an `#inform:` when called.
"
Class {
	#name : #InformDebugger,
	#superclass : #LogDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #testing }
InformDebugger class >> handlesContext: aContext [
	"Returns whether this debugger is suitable to debug @aContext"
	^ true
]

{ #category : #'debugger declaration' }
InformDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	<iAmADebugger>
]

{ #category : #'opening api' }
InformDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self inform: (self messageToPrintFrom: aDebugSession)
]

{ #category : #'debugger declaration' }
InformDebugger class >> rank [
	"Returns an int. When an exception needs to  be debugged, the rank is used to choose among the available debuggers. A higher rank means a higher priority."
	^ 10
]
