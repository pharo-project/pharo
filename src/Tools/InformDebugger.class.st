"
I am a debugger that will log the error in an `#inform:` when called.
"
Class {
	#name : #InformDebugger,
	#superclass : #LogDebugger,
	#category : #'Tools-Debugger'
}

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
InformDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is"
	^ 10
]
