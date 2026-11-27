"
I check the life of the SDL event loop: the process of `OSSDL2Driver`, shared by all the OS windows, that polls SDL and dispatches its events.

## Design Rationale

- The loop of a saved image ends by itself when the image starts again, in a new session, but `OSSDL2Driver` still references it. So the activation of a world tells the driver whether the image is starting: `WorldState>>worldRenderer:whenImageStarting:` reaches `OSWorldRenderer>>doActivateWhenImageStarting:`, which sends `startUp:` to the driver. When the image starts, the driver replaces the loop; otherwise, as when a native window opens, it keeps the running one. I check both cases, on the driver and through the activation of a world.
- The test environment terminates the processes a test forks. I check that the loop is not one of them, so that a test that replaces the loop does not leave the image without one.

## Implementation Notes

- The event loop is global state. When a loop was there before a test, `tearDown` makes sure one runs again; otherwise it shuts the loop down. Activating a world also makes a new default clipboard, so `tearDown` puts the previous one back and closes the window the test opened.
- I only run with an interactive image whose current driver is an `OSSDL2Driver`.
"
Class {
	#name : 'OSSDL2EventLoopTest',
	#superclass : 'TestCase',
	#instVars : [
		'driver',
		'hadEventLoop',
		'renderer',
		'previousClipboard'
	],
	#category : 'OSWindow-Tests-Tests',
	#package : 'OSWindow-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
OSSDL2EventLoopTest >> setUp [

	super setUp.
	driver := OSWindowDriver current.
	(Smalltalk isInteractive and: [ driver isKindOf: OSSDL2Driver ]) ifFalse: [ self skip ].
	hadEventLoop := driver eventLoopProcess isNotNil.
	previousClipboard := Clipboard default
]

{ #category : 'running' }
OSSDL2EventLoopTest >> tearDown [

	renderer ifNotNil: [ renderer deactivate ].
	(previousClipboard isKindOf: OSWindowClipboard) ifTrue: [ previousClipboard beDefault ].
	hadEventLoop ifNotNil: [
		hadEventLoop
			ifTrue: [ driver ensureEventLoop ]
			ifFalse: [ driver shutdownEventLoop ] ].
	super tearDown
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testActivatingAWorldWhenImageIsNotStartingKeepsTheLoop [

	| world loop |
	world := OSWindowWorldMorph new.
	renderer := OSWorldRenderer forWorld: world.
	driver forceNewEventLoop.
	loop := driver eventLoopProcess.
	world worldState worldRenderer: renderer whenImageStarting: false.
	self assert: driver eventLoopProcess identicalTo: loop
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testActivatingAWorldWhenImageStartsReplacesTheLoop [

	| world loop |
	world := OSWindowWorldMorph new.
	renderer := OSWorldRenderer forWorld: world.
	driver forceNewEventLoop.
	loop := driver eventLoopProcess.
	world worldState worldRenderer: renderer whenImageStarting: true.
	self deny: driver eventLoopProcess identicalTo: loop.
	self deny: driver eventLoopProcess isTerminated
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testEnsureEventLoopKeepsTheRunningLoop [

	| loop |
	driver forceNewEventLoop.
	loop := driver eventLoopProcess.
	driver ensureEventLoop.
	self assert: driver eventLoopProcess identicalTo: loop
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testEventLoopIsNotOwnedByTheTestThatStartedIt [
	"The test environment terminates the processes forked by a test.
	The event loop belongs to the whole image and must survive the test"

	driver forceNewEventLoop.
	self deny: (CurrentExecutionEnvironment value forkedProcesses includes: driver eventLoopProcess)
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testStartUpWhenImageIsNotStartingKeepsTheRunningLoop [

	| loop |
	driver forceNewEventLoop.
	loop := driver eventLoopProcess.
	driver startUp: false.
	self assert: driver eventLoopProcess identicalTo: loop
]

{ #category : 'tests' }
OSSDL2EventLoopTest >> testStartUpWhenImageStartsReplacesTheLoop [

	| loop |
	driver forceNewEventLoop.
	loop := driver eventLoopProcess.
	driver startUp: true.
	self deny: driver eventLoopProcess identicalTo: loop.
	self deny: driver eventLoopProcess isTerminated
]
