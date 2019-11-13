Class {
	#name : #TestDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'debugger declaration' }
TestDebugger class >> iAmADebugger [
	<iAmADebugger>
]

{ #category : #'opening api' }
TestDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	Transcript show: 'Opening MyDebugger'
]

{ #category : #'debugger declaration' }
TestDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is. Signal the DebugSessionCannotBeOpenedByThisDebugger exception if this debugger absolutely cannot debug @aDebugSession (for example if this debugger can only handle debug sessions from test executions, and @aDebugSession does not come from a test execution)"
	^ (aDebugSession name == 'ArithmeticError') ifTrue: [ ^ 150 ] ifFalse: [ DebugSessionCannotBeOpenedByThisDebugger signal ]
]
