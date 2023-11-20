"
An UserInterruptHandler watches incoming input events and checks for the user interrupt key.
If encountered it interrupts the current process and opens a notifier..

Instance Variables
	interruptKey:		<Object>

interruptKey
	- definition of the user interrupt key

"
Class {
	#name : 'UserInterruptHandler',
	#superclass : 'Object',
	#classVars : [
		'CmdDotEnabled'
	],
	#category : 'OSWindow-SDL2-Base',
	#package : 'OSWindow-SDL2',
	#tag : 'Base'
}

{ #category : 'setting' }
UserInterruptHandler class >> cmdDotEnabled [
	^ CmdDotEnabled ifNil: [CmdDotEnabled := true]
]

{ #category : 'setting' }
UserInterruptHandler class >> cmdDotEnabled: aBoolean [
	CmdDotEnabled := aBoolean
]

{ #category : 'setting' }
UserInterruptHandler class >> settingsOn: aBuilder [

	<systemsettings>

	(aBuilder setting: #cmdDotEnabled)
		parent: #debugging;
		label: 'Enable cmd-dot interrupt key';
		target: UserInterruptHandler;
		default: true;
		description:
			'If true, it allows the user to stop currently running process by opening-up a debugger'
]

{ #category : 'private' }
UserInterruptHandler >> handleUserInterrupt [
	"This will be called from the event-fetcher process.
	Assume no system-vital processes have a lower priority than this, and are thus ok to interrupt"
	UserInterruptHandler cmdDotEnabled ifFalse: [ ^ self ].

	[
		self processToInterrupt
			ifNotNil: [ :proc | proc debugWithTitle: 'User Interrupt' ]
			ifNil: [ self inform: 'Process not interruptable' ]

	"fork exceptions, we don't want interrupt handler to die"
	] on: Error fork: [:ex | ex pass]
]

{ #category : 'private' }
UserInterruptHandler >> processToInterrupt [
	"Look for best candidate to interrupt:
		- any scheduled non-finalization process of lower priority
		- the weak-finalization process, if scheduled
		- the UI process
	Never interrupt the idle process, since killing it is fatal"

	| fallback |
	fallback := UIManager default uiProcess.
	Processor
		scanSchedule: [:p |
		 "suspendedContext sender == nil usually means that process is only scheduled but had no chance to run"
			(p ~~ Processor backgroundProcess and: [p suspendedContext sender notNil]) ifTrue: [
				p ~~  FinalizationProcess runningFinalizationProcess
					ifTrue: [^p]
					ifFalse: [fallback := p]]]
		startingAt: Processor activePriority.
	^fallback
]
