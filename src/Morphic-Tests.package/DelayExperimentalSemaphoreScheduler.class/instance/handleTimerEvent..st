timer process
handleTimerEvent: microsecondNowTick
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| microsecondNextTick |

	"Process any schedule requests"
	scheduledDelay ifNotNil: [
		"Schedule the given delay."
		scheduledDelay scheduler: self resumptionTime: microsecondNowTick + (1000 * scheduledDelay millisecondDelayDuration).
		self scheduleDelay: scheduledDelay.
		scheduledDelay := nil.
		scheduledDelayIsNil consumeAllSignals. 
		scheduledDelayIsNil signal ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil.
		finishedDelayIsNil consumeAllSignals.
		finishedDelayIsNil signal ].

	"Signal any expired delays"
	[ activeDelay notNil and:[ microsecondNowTick >= activeDelay schedulerResumptionTime ]] whileTrue: [
		activeDelay schedulerSignalWaitingProcess.
		activeDelay := suspendedDelays removeFirstOrNil.
		].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	microsecondNextTick := microsecondNowTick + (1"sec" * 1000"msecs" * 1000"usecs").
	activeDelay ifNotNil: [
		microsecondNextTick := microsecondNextTick min: activeDelay schedulerResumptionTime ].
	^microsecondNextTick.
