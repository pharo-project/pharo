Class {
	#name : #EDDebuggingAPITest,
	#superclass : #EDTest,
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #helpers }
EDDebuggingAPITest >> forbiddenProcesses [
	self flag: 'This is a fixed list of processes we do not want to kill if they exist. This is too magickal, but we lack tools to have such information and we do not want to depend on process browser packages (information is taken from there).'.
	^ {[  ].
	[ Smalltalk lowSpaceWatcherProcess ].
	[ WeakArray runningFinalizationProcess ].
	[ Processor backgroundProcess ].
	[ InputEventFetcher default fetcherProcess ].
	[ UIManager default uiProcess ].
	[ Delay schedulingProcess ]}
]

{ #category : #helpers }
EDDebuggingAPITest >> injectContextWithMethodWithChanges [
	| context |
	self prepareMethodVersionTest.
	context := Context
		newForMethod: EDMockObjectForTests >> #mWithVersion.
	context
		setSender: nil
		receiver: nil
		method: EDMockObjectForTests >> #mWithVersion
		closure: nil
		startpc: nil.
	debugAPI stack addFirst: context
]

{ #category : #'tests - processes' }
EDDebuggingAPITest >> testAllRunningProcesses [
	| processes |
	processes := EDDebuggingAPI allRunningProcesses.
	self assertCollection: Process allSubInstances includesAll: processes.
	self
		denyCollection: processes
		includesAny: EDDebuggingAPI forbiddenProcesses.
	processes do: [ :p | self deny: p isTerminated ]
]

{ #category : #'tests - initialization' }
EDDebuggingAPITest >> testAttachTo [
	self assert: debugAPI session identicalTo: session
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testChangeRecordsForMethod [
	| changes |
	self prepareMethodVersionTest.
	changes := debugAPI
		changeRecordsForMethod: (EDMockObjectForTests >> #mWithVersion) asHistoricalRingDefinition.
	self assert: changes size equals: 2.
	self assert: changes first sourceCode equals: self sampleMethodSourceCodeVersion2.
	self assert: changes second sourceCode equals: self sampleMethodSourceCodeVersion1
]

{ #category : #'tests - debugging support' }
EDDebuggingAPITest >> testCloseEmergencySession [
	ed := EDEmergencyDebugger new.
	ed basicDebug: session.
	ed debugAPI closeEmergencySession.
	self deny: ed isRunning
]

{ #category : #'tests - context' }
EDDebuggingAPITest >> testContextAt [
	1 to: session stack size do: [ :i | 
		self
			assert: (debugAPI contextAt: i)
			identicalTo: (session stack at: i) ]
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testDisplayStackSize [
	self
		assert: debugAPI displayStackSize
		equals: debugAPI class displayStackSize
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testDumpStack [
	| dump splittedDump |
	dump := debugAPI dumpStack.
	splittedDump := dump splitOn: Character cr.
	self
		assertCollection: splittedDump
		equals: (session shortStack collect: [ :ctx | ctx printString ])
]

{ #category : #'tests - processes' }
EDDebuggingAPITest >> testForbiddenProcesses [
	EDDebuggingAPI forbiddenProcesses
		with: self forbiddenProcesses
		do:
			[ :actualProcess :expectedProcess | self assert: actualProcess value identicalTo: expectedProcess value ]
]

{ #category : #'tests - initialization' }
EDDebuggingAPITest >> testInitialize [
	self assert: debugAPI debugger isNil.
	self assertEmpty: debugAPI changesForMethods
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testLongStack [
	self
		assertCollection: debugAPI longStack
		equals: (session stackOfSize: debugAPI longStackSize)
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testLongStackSize [
	self
		assert: debugAPI longStackSize
		equals: debugAPI class longStackSize
]

{ #category : #'tests - context' }
EDDebuggingAPITest >> testMethodAt [
	1 to: session stack size do: [ :i | 
		self
			assert: (debugAPI methodAt: i)
			identicalTo: (session stack at: i) method ]
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testMethodVersionAt [
	self injectContextWithMethodWithChanges.
	self
		assert: (debugAPI methodVersionAt: 1)
		equals: (debugAPI changesForMethods at: EDMockObjectForTests >> #mWithVersion)
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testMethodVersionSizeAt [
	self injectContextWithMethodWithChanges.
	self assert: (debugAPI methodVersionAt: 1) size equals: 2
]

{ #category : #'tests - context' }
EDDebuggingAPITest >> testNodeAt [
	1 to: session stack size do: [ :i | 
		| context |
		context := session stack at: i.
		self
			assert: (debugAPI nodeAt: i)
			identicalTo: (context method sourceNodeForPC: context pc) ]
]

{ #category : #'tests - context' }
EDDebuggingAPITest >> testNodeForContext [
	| context |
	context := session stack first.
	self
		assert: (debugAPI nodeForContext: context)
		identicalTo: (context method sourceNodeForPC: context pc)
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testResetStack [
	debugAPI longStack removeAll.
	debugAPI resetStack.
	self
		assertCollection: debugAPI longStack
		equals: (session stackOfSize: debugAPI longStackSize)
]

{ #category : #'tests - context' }
EDDebuggingAPITest >> testRestartContextAt [
	| secondContext |
	secondContext := debugAPI longStack second.
	debugAPI restartContextAt: 2.
	self assert: debugAPI longStack first identicalTo: secondContext
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testRevertTo [
	|method changes|
	self prepareMethodVersionTest.
	method := (EDMockObjectForTests >> #mWithVersion).
	changes := debugAPI changeRecordsForMethod: method.
		
	self assert: method sourceCode equals: self sampleMethodSourceCodeVersion2.
	debugAPI revert: (EDMockObjectForTests >> #mWithVersion) to: changes second.
	
	"Method version is now the reverted one"
	method := (EDMockObjectForTests >> #mWithVersion).
	self assert: method sourceCode equals: self sampleMethodSourceCodeVersion1.
	
	"Changes now include new version, equals to the first one"
	changes := debugAPI changeRecordsForMethod: method. 
	self assert: changes size equals: 3.
	self assert: changes first sourceCode equals: self sampleMethodSourceCodeVersion1.
	self assert: changes second sourceCode equals: self sampleMethodSourceCodeVersion2.
	self assert: changes third sourceCode equals: self sampleMethodSourceCodeVersion1.
	
	"Changes cache should be updated: 
	we test for source code version, as changes cannot compare otherwise than using identity check =="
	self assert: (debugAPI versionsForMethod: method) size equals: 3.
	self assertCollection: ((debugAPI versionsForMethod: method) collect:[:c| c sourceCode])
				equals: (changes collect:[:c| c sourceCode])
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testRevertToInContext [
	|method changes|
	self prepareMethodVersionTest.
	method := (EDMockObjectForTests >> #mWithVersion).
	changes := debugAPI changeRecordsForMethod: method.
		
	self assert: method sourceCode equals: self sampleMethodSourceCodeVersion2.
	debugAPI revert: (EDMockObjectForTests >> #mWithVersion) 
				to: changes second 
				inContext: debugAPI stack first.
	
	"Method version is now the reverted one"
	method := (EDMockObjectForTests >> #mWithVersion).
	self assert: method sourceCode equals: self sampleMethodSourceCodeVersion1.
	
	"Stack is updated"
	self assertCollection: debugAPI longStack 
		  equals: (debugAPI session stackOfSize: debugAPI longStackSize).
	
	"Changes now include new version, equals to the first one"
	changes := debugAPI changeRecordsForMethod: method. 
	self assert: changes size equals: 3.
	self assert: changes first sourceCode equals: self sampleMethodSourceCodeVersion1.
	self assert: changes second sourceCode equals: self sampleMethodSourceCodeVersion2.
	self assert: changes third sourceCode equals: self sampleMethodSourceCodeVersion1.
	
	"Changes cache should be updated: 
	we test for source code version, as changes cannot compare otherwise than using identity check =="
	self assert: (debugAPI versionsForMethod: method) size equals: 3.
	self assertCollection: ((debugAPI versionsForMethod: method) collect:[:c| c sourceCode])
				equals: (changes collect:[:c| c sourceCode])
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testStack [ 
	self assert: debugAPI stack identicalTo: debugAPI longStack
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testStackAt [
	1 to: session stack size do:
		[ :i | self assert: (debugAPI stackAt: i) equals: (session stack at: i) ]
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testStackFromTo [
	| stack stackExtract |
	stack := session stack.
	stackExtract := (stack copyFrom: 5 to: 10).
	self
		assertCollection: (debugAPI stackFrom: 5 to: 10)
		equals: stackExtract.
		
	stackExtract := (stack copyFrom: (stack size - 2) to: (stack size)).
	self
		assertCollection: (debugAPI stackFrom: (stack size - 2) to: (stack size + 5))
		equals: stackExtract.
		
	
]

{ #category : #'tests - stack' }
EDDebuggingAPITest >> testStackSize [
	self
		assert: debugAPI stackSize
		equals: debugAPI stack size
]

{ #category : #'tests - processes' }
EDDebuggingAPITest >> testTerminateProcesses [
	|processes|
	processes := { [  ] newProcess.  [  ] newProcess.}.
	processes do:[:p| self deny: p isTerminated].
	
	EDDebuggingAPI terminateProcesses: processes.
	processes do:[:p| self assert: p isTerminated].
]

{ #category : #'tests - debugging support' }
EDDebuggingAPITest >> testTerminateSession [

	|process|
	ed := EDEmergencyDebugger new.
	ed basicDebug: session.
	process := ed debugAPI session interruptedProcess.
	ed debugAPI terminateSession.
	self assert: ed debugAPI session interruptedProcess isNil.
	self assert: process isTerminated
]

{ #category : #'tests - reverting methods' }
EDDebuggingAPITest >> testVersionsForMethod [
	|changes|
	self prepareMethodVersionTest.
	changes := debugAPI versionsForMethod: (EDMockObjectForTests >> #mWithVersion).	
	self assert: debugAPI changesForMethods size equals: 1.
	self assert: (debugAPI changesForMethods at: (EDMockObjectForTests >> #mWithVersion))
			identicalTo: changes
]
