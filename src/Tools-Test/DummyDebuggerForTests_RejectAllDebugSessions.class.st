"
I am a copy of class DummyDebuggerForTests_CannotOpenDebugSession. This comment is copied from there, and might not be entirely accurate

Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, ""I represent a paragraph of text"".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points
"
Class {
	#name : #'DummyDebuggerForTests_RejectAllDebugSessions',
	#superclass : #AbstractDebugger,
	#category : #'Tools-Test-Debugger'
}

{ #category : #'debugger declaration' }
DummyDebuggerForTests_RejectAllDebugSessions class >> iAmADebugger [
	<iAmADebugger> <iAmADummyDebuggerForTest>
]

{ #category : #'opening api' }
DummyDebuggerForTests_RejectAllDebugSessions class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"This method will be called to open this debugger on a given debug session (i.e. execution to debug). If this debugger wants to keep the debug session (and its process) alive (for example because it is a graphical debugger that would break if the debug session is terminated while its window is open), it should call #keepAlive: on @aDebugSession, passing itself as argument.
	The contract is that if a debugger calls #keepAlive:, it should also call #stopKeepingAlive: on @aDebugSession when it closes (passing itself as argument again).
	The debug session will automatically not be kept alive by this debugger anymore if this debugger gets garbage collected (and DebugSession will only hold a weak reference to this debugger, so it will not prevent its garbage collection)"
	"Do nothing"
]

{ #category : #'debugger declaration' }
DummyDebuggerForTests_RejectAllDebugSessions class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is. Signal the DebugSessionCannotBeOpenedByThisDebugger exception if this debugger absolutely cannot debug @aDebugSession (for example if this debugger can only handle debug sessions from test executions, and @aDebugSession does not come from a test execution)"
	DebugSessionCannotBeOpenedByThisDebugger signal
]
