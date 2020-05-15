Class {
	#name : #EDTest,
	#superclass : #TestCase,
	#instVars : [
		'ed',
		'session',
		'debugAPI',
		'mockDisplayAPI',
		'currentDebuggingAPI'
	],
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #testing }
EDTest class >> isAbstract [ 	
	^self == EDTest 
]

{ #category : #running }
EDTest >> configureDebugSession [

	"Configure a debug session and moves it to the following stack:
	EDMockObjectForTests>>m: 5
	EDMockObjectForTests>>m: 6
	EDMockObjectForTests>>m: 7
	EDMockObjectForTests>>m: 8
	EDMockObjectForTests>>m: 9
	EDMockObjectForTests>>m: 10
	EDMockObjectForTests>>m: 11
	EDMockObjectForTests>>m: 12
	EDMockObjectForTests>>m: 13
	EDMockObjectForTests>>m: 14
	EDMockObjectForTests>>m: 15
	EDMockObjectForTests>>m: 16
	EDMockObjectForTests>>m: 17
	EDMockObjectForTests>>m: 18
	EDMockObjectForTests>>m: 19
	EDMockObjectForTests>>m: 20
	[ EDMockObjectForTests new m: 20 ] in UndefinedObject>>DoIt"
	
	session stepThrough.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.	
	session stepOver.
	session stepOver.
	session stepInto.
	
	session stepOver.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.	
	session stepOver.
	session stepOver.
	session stepInto.
	
	session stepOver.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.	
	session stepOver.
	session stepOver.
	session stepInto.
	
	session stepOver.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.	
	session stepOver.
	session stepOver.
	session stepInto.
	
	session stepOver.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.	
	session stepOver.
	session stepOver.
	session stepInto.

]

{ #category : #'instance creation' }
EDTest >> newEd [
	ed := EDEmergencyDebugger new.
	ed repl: EDMockREPLInterface new.
	ed displayInterface: (mockDisplayAPI := EDMockDisplayInterface new).
	ed debugAPI: debugAPI.
	ed debug: session
]

{ #category : #'methods version' }
EDTest >> prepareMethodVersionTest [
	| context process |
	EDMockObjectForTests compile: self sampleMethodSourceCodeVersion1.
	EDMockObjectForTests compile: self sampleMethodSourceCodeVersion2.

	context := [ EDMockObjectForTests new mWithVersion] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	session := DebugSession
		named: 'test session'
		on: process
		startedAt: context.
	session stepThrough.
	session stepOver.
	session stepInto.
	session stepOver.
	debugAPI session: session.
	self newEd
]

{ #category : #'methods version' }
EDTest >> sampleMethodSourceCodeVersion1 [
	^ 'mWithVersion |i| i := 0. ^i + 1'
]

{ #category : #'methods version' }
EDTest >> sampleMethodSourceCodeVersion2 [
	^ 'mWithVersion |i| i := 0. ^i + 2'
]

{ #category : #running }
EDTest >> setUp [
	|context process|
	super setUp.
	currentDebuggingAPI := EDEmergencyDebugger debugAPI.
	context := [ EDMockObjectForTests new m: 20 ] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	session := DebugSession
		named: 'test session'
		on: process
		startedAt: context.
	self configureDebugSession.
	debugAPI := EDDebuggingAPI attachTo: session.
	EDEmergencyDebugger debugAPI: nil.
	self newEd
]

{ #category : #running }
EDTest >> tearDown [
	EDEmergencyDebugger debugAPI: currentDebuggingAPI.
	session terminate.
	EDMockObjectForTests removeSelector: #mWithVersion.
	super tearDown
]
