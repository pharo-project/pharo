"
I check what `OSSDL2Driver` does with its event loop when the image starts, resumes or shuts down.

## Design Rationale

- The loop of a saved image ends by itself when the image starts again, but the driver still references it. `OSSDL2Driver>>startUp: true` must replace it; `startUp: false`, sent when a native window opens or the image resumes after a save, must keep the running one.
- I use an `OSSDL2DriverMock` with a runner of its own: I touch neither the image's event loop nor SDL, so I run in a headless image too.
"
Class {
	#name : 'OSSDL2DriverEventLoopTest',
	#superclass : 'TestCase',
	#instVars : [
		'driver',
		'runner'
	],
	#category : 'OSWindow-Tests-Tests',
	#package : 'OSWindow-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
OSSDL2DriverEventLoopTest >> setUp [

	super setUp.
	runner := OSSDL2EventLoopRunner new.
	driver := OSSDL2DriverMock new
		          eventLoopRunner: runner;
		          yourself
]

{ #category : 'running' }
OSSDL2DriverEventLoopTest >> tearDown [

	runner stop.
	super tearDown
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testShutDownStopsTheLoop [

	driver ensureEventLoop.
	driver shutDown: false.
	self deny: runner isRunning
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testStartUpWhenImageIsNotStartingKeepsTheLoop [

	| loop |
	driver ensureEventLoop.
	loop := driver eventLoopProcess.
	driver startUp: false.
	self assert: driver eventLoopProcess identicalTo: loop
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testStartUpWhenImageIsNotStartingStartsAMissingLoop [

	driver startUp: false.
	self assert: runner isRunning
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testStartUpWhenImageStartsReplacesTheLoop [

	| loop |
	driver ensureEventLoop.
	loop := driver eventLoopProcess.
	driver startUp: true.
	self assert: loop isTerminated.
	self assert: runner isRunning
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testUsesTheImageRunnerByDefault [

	self assert: OSSDL2DriverMock new eventLoopRunner identicalTo: OSSDL2Driver sharedEventLoopRunner
]

{ #category : 'tests' }
OSSDL2DriverEventLoopTest >> testUsesTheRunnerItIsGiven [

	driver ensureEventLoop.
	self assert: driver eventLoopProcess identicalTo: runner process
]
