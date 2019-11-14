"
I am an abstract class to define the minimal API of the Debugger.

My subclasses can be registred like this:

	NoDebugger beCurrent
"
Class {
	#name : #AbstractDebugger,
	#superclass : #Object,
	#category : #'Tools-Debugger'
}

{ #category : #query }
AbstractDebugger class >> beCurrent [
	self isAbstract ifTrue: [ self error: 'Cannot register an abstract debugger' ].
	
	Debugger mainDebuggerClass: self
]

{ #category : #'debugger declaration' }
AbstractDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	self subclassResponsibility 
]

{ #category : #testing }
AbstractDebugger class >> isAbstract [
	^ self = AbstractDebugger
]

{ #category : #'opening api' }
AbstractDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"This method will be called to open this debugger on a given debug session (i.e. execution to debug). If this debugger wants to keep the debug session (and its process) alive (for example because it is a graphical debugger that would break if the debug session is terminated while its window is open), it should call #keepAlive: on @aDebugSession, passing itself as argument.
	The contract is that if a debugger calls #keepAlive:, it should also call #stopKeepingAlive: on @aDebugSession when it closes (passing itself as argument again).
	The debug session will automatically not be kept alive by this debugger anymore if this debugger gets garbage collected (and DebugSession will only hold a weak reference to this debugger, so it will not prevent its garbage collection)"
	self subclassResponsibility
]

{ #category : #'debugger declaration' }
AbstractDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is. Signal the DebugSessionCannotBeOpenedByThisDebugger exception if this debugger absolutely cannot debug @aDebugSession (for example if this debugger can only handle debug sessions from test executions, and @aDebugSession does not come from a test execution)"
	self subclassResponsibility
]
