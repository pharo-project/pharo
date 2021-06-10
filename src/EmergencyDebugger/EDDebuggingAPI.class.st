"
I am the default ED Debugging interface.
I return a set of CmCommands exposing my interface.
"
Class {
	#name : #EDDebuggingAPI,
	#superclass : #Object,
	#instVars : [
		'session',
		'debugger',
		'changesForMethods',
		'longStack'
	],
	#category : #'EmergencyDebugger-Core'
}

{ #category : #'API - Processes' }
EDDebuggingAPI class >> allRunningProcesses [
	^ Process allSubInstances
		reject: [ :each | 
			each isTerminated
				or: [ self forbiddenProcesses anySatisfy: [ :block | block value == each ] ] ]
]

{ #category : #start }
EDDebuggingAPI class >> attachTo: aDebugSession [
	^ self new
		session: aDebugSession;
		yourself
]

{ #category : #accessing }
EDDebuggingAPI class >> displayStackSize [
	^5
]

{ #category : #'API - Processes' }
EDDebuggingAPI class >> forbiddenProcesses [
	^ {[  ].
	[ Smalltalk lowSpaceWatcherProcess ].
	[ WeakArray runningFinalizationProcess ].
	[ Processor backgroundProcess ].
	[ InputEventFetcher default fetcherProcess ].
	[ UIManager default uiProcess ].
	[ Delay schedulingProcess ]}
]

{ #category : #accessing }
EDDebuggingAPI class >> longStackSize [
	^100
]

{ #category : #'API - Processes' }
EDDebuggingAPI class >> terminateAllProcesses [
	self terminateProcesses: self allRunningProcesses
]

{ #category : #'API - Processes' }
EDDebuggingAPI class >> terminateProcesses: processes [
	processes do: [ :process | process ifNotNil: [ process terminate ] ]
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> changeRecordsForMethod: aCompiledMethod [
	^ SourceFiles
		changeRecordsFrom: aCompiledMethod sourcePointer
		className: aCompiledMethod methodClass name
		isMeta: aCompiledMethod methodClass isMeta
]

{ #category : #accessing }
EDDebuggingAPI >> changesForMethods [
	^changesForMethods
]

{ #category : #'API - Debugging support' }
EDDebuggingAPI >> closeEmergencySession [

	debugger terminate
	
]

{ #category : #'API - Context' }
EDDebuggingAPI >> contextAt: index [
	^self stackAt: index
]

{ #category : #accessing }
EDDebuggingAPI >> debugger [
	^debugger
]

{ #category : #accessing }
EDDebuggingAPI >> debugger: anObject [
	debugger := anObject
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> displayStackSize [
	^self class displayStackSize
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> dumpStack [

	| str |
	str := String new writeStream.
	self stack printOn: str delimiter: String cr.
	^ str contents
]

{ #category : #initialization }
EDDebuggingAPI >> initialize [
	changesForMethods := Dictionary new
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> longStack [
	^ longStack
		ifNil: [ longStack := session stackOfSize: self longStackSize ]
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> longStackSize [
	^self class longStackSize
]

{ #category : #'API - Context' }
EDDebuggingAPI >> methodAt: index [
	^(self contextAt: index) method
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> methodVersionAt: index [
	^ self versionsForMethod: (self methodAt: index)
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> methodVersionSizeAt: index [
	^ (self versionsForMethod: (self methodAt: index)) size
]

{ #category : #'API - Context' }
EDDebuggingAPI >> nodeAt: index [
	^ self nodeForContext: (self contextAt: index)
]

{ #category : #'API - Context' }
EDDebuggingAPI >> nodeForContext: aContext [
	^ aContext method sourceNodeForPC: aContext pc
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> resetStack [
	longStack := nil
]

{ #category : #'API - Context' }
EDDebuggingAPI >> restartContextAt: anIndex [
	session restart: (self contextAt: anIndex).
	self resetStack
]

{ #category : #'API - Debugging support' }
EDDebuggingAPI >> resume [
	self closeEmergencySession.
	session resume
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> revert: aCompiledMethod to: aChangeRecord [
	"Reverts the given method to the given change record"

	aCompiledMethod methodClass
		compile: aChangeRecord sourceCode
		classified: aChangeRecord category
		withStamp: aChangeRecord stamp
		notifying: nil
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> revert: aCompiledMethod to: aChangeRecord inContext: aContext [
	"Reverts the given method to the given change record in the given context. 
	The cached stack is updated."

	session
		recompileMethodTo: aChangeRecord sourceCode
		inContext: aContext
		notifying: nil.
	longStack := nil
]

{ #category : #accessing }
EDDebuggingAPI >> session [
	^session
]

{ #category : #accessing }
EDDebuggingAPI >> session: anObject [
	session := anObject
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> stack [ 	
	^self longStack
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> stackAt: index [
	^self stack at: index
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> stackFrom: start to: stop [
	^ self longStack copyFrom: (start max: 0) to: (stop min: session stack size)
]

{ #category : #'API - Stack' }
EDDebuggingAPI >> stackSize [
	^ self longStack size
]

{ #category : #'API - Processes' }
EDDebuggingAPI >> terminateAllProcesses [
	self terminateSession.
	self class terminateAllProcesses
]

{ #category : #'API - Debugging support' }
EDDebuggingAPI >> terminateSession [
	self closeEmergencySession.
	session terminate
]

{ #category : #'API - Debugging support' }
EDDebuggingAPI >> tryReopenSessionWithDebugger [

	self closeEmergencySession.

	(OupsDebugRequest newForContext: session interruptedContext)
		process: session interruptedProcess;
		submit
]

{ #category : #'API - Revert' }
EDDebuggingAPI >> versionsForMethod: aCompiledMethod [
	^ changesForMethods
		at: aCompiledMethod
		ifAbsentPut: [ self changeRecordsForMethod: aCompiledMethod ]
]
