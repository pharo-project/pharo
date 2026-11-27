"
I check how `OSSDL2EventLoopRunner` starts, keeps, replaces and stops a loop.

## Design Rationale

- Each test has a runner of its own, running a loop that only waits: I touch neither the image's event loop nor SDL, so I run in a headless image too.
"
Class {
	#name : 'OSSDL2EventLoopRunnerTest',
	#superclass : 'TestCase',
	#instVars : [
		'runner'
	],
	#category : 'OSWindow-Tests-Tests',
	#package : 'OSWindow-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
OSSDL2EventLoopRunnerTest >> setUp [

	super setUp.
	runner := OSSDL2EventLoopRunner new
]

{ #category : 'running' }
OSSDL2EventLoopRunnerTest >> tearDown [

	runner stop.
	super tearDown
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testEnsureRunningKeepsTheRunningLoop [

	| loop |
	runner ensureRunning: [ Semaphore new wait ].
	loop := runner process.
	runner ensureRunning: [ Semaphore new wait ].
	self assert: runner process identicalTo: loop
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testEnsureRunningReplacesAnEndedLoop [

	| loop |
	runner ensureRunning: [ Semaphore new wait ].
	loop := runner process.
	loop terminate.
	runner ensureRunning: [ Semaphore new wait ].
	self deny: runner process identicalTo: loop.
	self assert: runner isRunning
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testEnsureRunningStartsALoop [

	runner ensureRunning: [ Semaphore new wait ].
	self assert: runner isRunning
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testIsNotRunningWhenNew [

	self deny: runner isRunning
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testLoopIsNamed [

	runner ensureRunning: [ Semaphore new wait ].
	self assert: runner process name equals: 'SDL2 Event loop'
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testRestartRunningReplacesTheRunningLoop [

	| loop |
	runner ensureRunning: [ Semaphore new wait ].
	loop := runner process.
	runner restartRunning: [ Semaphore new wait ].
	self assert: loop isTerminated.
	self assert: runner isRunning
]

{ #category : 'tests' }
OSSDL2EventLoopRunnerTest >> testStopTerminatesTheLoop [

	| loop |
	runner ensureRunning: [ Semaphore new wait ].
	loop := runner process.
	runner stop.
	self assert: loop isTerminated.
	self deny: runner isRunning
]
