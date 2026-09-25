"
I am a trait implementing the main behavior of a task scheduler. That is, I define as requirement a #schedule: message and I define #future: on top of #schedule:.
"
Trait {
	#name : 'TTaskScheduler',
	#category : 'TaskIt-Kernel',
	#package : 'TaskIt',
	#tag : 'Kernel'
}

{ #category : 'testing' }
TTaskScheduler classSide >> isDebuggingCompatible [
	^ false
]

{ #category : 'scheduling' }
TTaskScheduler >> future: aTask [
	^ aTask asTask future: self
]

{ #category : 'scheduling' }
TTaskScheduler >> isDebuggingCompatible [
	^ false
]

{ #category : 'scheduling' }
TTaskScheduler >> isLocalThreadRunner [
	^ false
]

{ #category : 'scheduling' }
TTaskScheduler >> isUIRunner [
	^ false
]

{ #category : 'scheduling' }
TTaskScheduler >> isWorker [
	^ false
]

{ #category : 'scheduling' }
TTaskScheduler >> privateFuture: aTask [
	| execution |
	execution := aTask asTask futureTaskExecutionFor: self.
	self scheduleTaskExecution: execution.
	^ execution future
]

{ #category : 'scheduling' }
TTaskScheduler >> privateSchedule: aTask [
	| execution |
	execution := aTask asTask scheduleTaskExecutionFor: self.
	self scheduleTaskExecution: execution
]

{ #category : 'scheduling' }
TTaskScheduler >> privateTimeout: timeout on: aTaskExecution [
	timeout wait.
	aTaskExecution isFinished
		ifFalse: [ aTaskExecution cancelWithReason: 'Timeout after: ' , timeout asString ]
]

{ #category : 'scheduling' }
TTaskScheduler >> schedule: aTask [
	aTask asTask schedule: self
]

{ #category : 'scheduling' }
TTaskScheduler >> schedule: aTask timeout: aTimeout [

	| execution |
	execution := TKTTimeoutableTaskExecution new.
	execution runner: self.
	execution task: aTask.
	execution timeout: aTimeout.

	self scheduleTaskExecution: execution
]

{ #category : 'scheduling' }
TTaskScheduler >> scheduleTaskExecution: aTaskExecution [

	self explicitRequirement
]

{ #category : 'scheduling' }
TTaskScheduler >> scheduleTimeout: timeout on: aTaskExecution [
	TKTConfiguration processProvider
		createProcessDoing:
			(MessageSend
				receiver: self
				selector: #privateTimeout:on:
				arguments:
					{timeout.
					aTaskExecution})
]
