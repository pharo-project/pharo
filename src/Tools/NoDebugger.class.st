"
I am a debugger that will do nothing in case I am called.
"
Class {
	#name : #NoDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'debugger declaration' }
NoDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	<iAmADebugger>
]

{ #category : #'opening api' }
NoDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"Do nothing"
]

{ #category : #'debugger declaration' }
NoDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is"
	^ 5
]
