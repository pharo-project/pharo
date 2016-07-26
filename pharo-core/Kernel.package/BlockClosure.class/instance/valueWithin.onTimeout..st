evaluating
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
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
				ifFalse:[ e pass]].