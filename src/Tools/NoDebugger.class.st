"
I am a debugger that will do nothing in case I am called.
"
Class {
	#name : #NoDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #testing }
NoDebugger class >> handlesContext: aContext [
	"Returns whether this debugger is suitable to debug @aContext"
	^ true
]

{ #category : #'opening api' }
NoDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	<iAmADebugger>
]

{ #category : #'opening api' }
NoDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"Do nothing"
]

{ #category : #'as yet unclassified' }
NoDebugger class >> rank [
	"Returns an int. When an exception needs to  be debugged, the rank is used to choose among the available debuggers. A higher rank means a higher priority."
	^ 5
]
