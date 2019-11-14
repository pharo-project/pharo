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
	"This method will be called to open this debugger on a given debug session (i.e. execution to debug). If this debugger wants to keep the debug session (and its process) alive (for example because it is a graphical debugger that would break if the debug session is terminated while its window is open), it should call #keepAlive: on @aDebugSession, passing itself as argument. The debug session will automatically not be kept alive by this debugger anymore if this debugger gets garbage collected (and DebugSession will only hold a weak reference to this debugger, so it will not prevent its garbage collection)"
	self inform: 'Opening TestDebugger on debugSession: ', aDebugSession name.
]

{ #category : #'debugger declaration' }
TestDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is. Signal the DebugSessionCannotBeOpenedByThisDebugger exception if this debugger absolutely cannot debug @aDebugSession (for example if this debugger can only handle debug sessions from test executions, and @aDebugSession does not come from a test execution)"
	^ (aDebugSession name = 'ArithmeticError') ifTrue: [ ^ 150 ] ifFalse: [ DebugSessionCannotBeOpenedByThisDebugger signal ]
]
