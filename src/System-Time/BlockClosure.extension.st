Extension { #name : 'BlockClosure' }

{ #category : '*System-Time' }
BlockClosure >> microsecondsToRun [
	"Answer the number of milliseconds the receiver takes to return its value."

	| initialMicroseconds |
	initialMicroseconds := Time microsecondClockValue.
	self value.
	^ Time microsecondClockValue - initialMicroseconds
]

{ #category : '*System-Time' }
BlockClosure >> millisecondsToRun [
	"Answer the number of milliseconds the receiver takes to return its value."

	^ self microsecondsToRun // 1e3
]

{ #category : '*System-Time' }
BlockClosure >> timeToRun [
	"Answer the duration taken to execute this block."

	^ Duration milliSeconds: self millisecondsToRun
]

{ #category : '*System-Time' }
BlockClosure >> valueWithin: aDuration onTimeout: timeoutBlock [
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	<debuggerCompleteToSender>
	^ self
		valueWithinMilliseconds: aDuration asMilliSeconds
		onTimeout: timeoutBlock
]

{ #category : '*System-Time' }
BlockClosure >> valueWithinMilliseconds: milliseconds onTimeout: timeoutBlock [
	"Evaluate the receiver.
	If the evaluation does not complete in less than milliseconds evaluate the timeoutBlock instead"
	<debuggerCompleteToSender>

	| theProcess delay watchdog tag |
	milliseconds <= 0 ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := Delay forMilliseconds: milliseconds.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)]
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e |
			e tag == tag
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]]
]
