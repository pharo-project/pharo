Extension { #name : 'ProcessMonitorTestServiceTest' }

{ #category : '*Debugging-Utils-Tests' }
ProcessMonitorTestServiceTest >> testAlwaysPassBackgroundHalt [

	| process haltWasPassed halt |
	haltWasPassed := false.
	halt := Halt new.
	process := self fork: [
			           Processor activeProcess suspend.
			           halt signal ].
	self testService handleNewProcess: process.

	process
		on: Halt
		do: [ :actual |
				actual == halt ifFalse: [ actual pass ]. "it allows to debug if it works wrongly"
				self assert: actual equals: halt.
				haltWasPassed := true ].
	process resume.

	self assert: haltWasPassed.
	self assert: process isTerminated description: 'process should be terminated now'.
	self deny: (self testService suspendedBackgroundFailures includesKey: process)
]

{ #category : '*Debugging-Utils-Tests' }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenHaltIsSignaled [

	| executed |
	executed := false.
	self fork: [
			self testService suspendBackgroundFailure: Error new.
			executed := true ].

	self testService handleException: Halt new.

	self assert: executed.
	self deny: self testService shouldSuspendBackgroundFailures
]

{ #category : '*Debugging-Utils-Tests' }
ProcessMonitorTestServiceTest >> testResumeFailedProcessesWhenHaltIsSignaledInBackground [

	| processWithHalt executed |
	executed := false.
	self fork: [
			self testService suspendBackgroundFailure: Error new.
			executed := true ].
	processWithHalt := self fork: [
			                   Processor activeProcess suspend.
			                   Halt new signal ].

	processWithHalt
		on: Halt
		do: [ :actual |  ].
	processWithHalt resume.

	self assert: executed.
	self deny: self testService shouldSuspendBackgroundFailures
]
