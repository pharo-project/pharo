Class {
	#name : #SindarinUILessDebugger,
	#superclass : #Object,
	#category : #'Sindarin-Base'
}

{ #category : #'debugger declaration' }
SindarinUILessDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	Smalltalk tools inspector openOn: (SindarinDebugger attachTo: aDebugSession)
]

{ #category : #'debugger declaration' }
SindarinUILessDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is. Signal the DebugSessionCannotBeOpenedByThisDebugger exception if this debugger absolutely cannot debug @aDebugSession (for example if this debugger can only handle debug sessions from test executions, and @aDebugSession does not come from a test execution)"
	
	^ 25
]
