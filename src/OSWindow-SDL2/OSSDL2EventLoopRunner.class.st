"
I run the SDL event loop: the process, shared by all the OS windows, that polls SDL and dispatches its events. I start it, keep it, replace it and stop it; `OSSDL2Driver` tells me what the loop does (`OSSDL2Driver>>eventLoop`).

## Examples

```
OSSDL2EventLoopRunner exampleRunningLoop
```

## Design Rationale

- The loop belongs to the image, not to the driver nor to the code that started it. `OSSDL2Driver class>>sharedEventLoopRunner` keeps one of me for the image, and every driver uses it unless it was given another one with `OSSDL2Driver>>eventLoopRunner:`. A test can thus run a loop of its own without touching the image's.
- `ensureRunning:` keeps a running loop, and replaces one that ended. `restartRunning:` always replaces it: when the image starts, the loop of the saved session is from another session and must go, whether it already ended or not.

## Usage

1. `OSSDL2Driver class>>sharedEventLoopRunner` creates the image's runner on first use.
2. `OSSDL2Driver>>ensureEventLoop`, `forceNewEventLoop` and `shutdownEventLoop` send me `ensureRunning:`, `restartRunning:` and `stop`, with `[ self eventLoop ]` as the loop to run. `OSSDL2Driver>>startUp:` and `shutDown:` send those when the image starts, resumes or shuts down.
3. `OSSDL2Driver class>>eventLoopProcess` answers my `process`, for instance to `ReleaseTest>>knownProcesses`.

I am mutable and not meant to be used from several processes at once: the drivers send me their messages from the process that starts or activates the UI.
"
Class {
	#name : 'OSSDL2EventLoopRunner',
	#superclass : 'Object',
	#instVars : [
		'process'
	],
	#category : 'OSWindow-SDL2-Base',
	#package : 'OSWindow-SDL2',
	#tag : 'Base'
}

{ #category : 'examples' }
OSSDL2EventLoopRunner class >> exampleRunningLoop [
	"A runner running a loop that only waits, inspected: its process is named 'SDL2 Event loop'.
	Send it stop to end the loop."

	<example>
	| runner |
	runner := self instanceStopped.
	runner ensureRunning: [ Semaphore new wait ].
	^ runner inspect
]

{ #category : 'instances' }
OSSDL2EventLoopRunner class >> instanceStopped [

	<sampleInstance>
	^ self new
]

{ #category : 'running' }
OSSDL2EventLoopRunner >> ensureRunning: aLoopBlock [
	"Keep the running loop. Start aLoopBlock as the loop if none runs"

	self isRunning ifTrue: [ ^ self ].
	self restartRunning: aLoopBlock
]

{ #category : 'testing' }
OSSDL2EventLoopRunner >> isRunning [

	^ process isNotNil and: [ process isTerminated not ]
]

{ #category : 'accessing' }
OSSDL2EventLoopRunner >> process [

	^ process
]

{ #category : 'running' }
OSSDL2EventLoopRunner >> restartRunning: aLoopBlock [
	"Stop the loop, then start aLoopBlock as the new one"

	self stop.
	process := aLoopBlock forkAt: Processor lowIOPriority named: 'SDL2 Event loop'
]

{ #category : 'running' }
OSSDL2EventLoopRunner >> stop [

	process ifNil: [ ^ self ].
	process terminate.
	process := nil
]
