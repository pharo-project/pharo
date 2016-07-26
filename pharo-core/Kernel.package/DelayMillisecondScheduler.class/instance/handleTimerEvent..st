timer process
handleTimerEvent: millisecondNowTick
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| millisecondNextTick |

	"Process any schedule requests"
	scheduledDelay ifNotNil: [
		"Schedule the given delay."
		scheduledDelay scheduler: self resumptionTime: millisecondNowTick + scheduledDelay millisecondDelayDuration.
		self scheduleDelay: scheduledDelay.
		scheduledDelay := nil ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil ].

	"Check for clock wrap-around."
	millisecondNowTick < activeDelayStartTime ifTrue: [
		"clock wrapped"
		self saveResumptionTimes.
		self restoreResumptionTimes ].
	activeDelayStartTime := millisecondNowTick.

	"Signal any expired delays"
	[ activeDelay notNil and:[ millisecondNowTick >= activeDelay schedulerResumptionTime ]] whileTrue: [
		activeDelay schedulerSignalWaitingProcess.
		activeDelay := suspendedDelays removeFirstOrNil.
		].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	millisecondNextTick := millisecondNowTick + 1000"msecs".
	activeDelay ifNotNil: [
		millisecondNextTick := millisecondNextTick min: activeDelay schedulerResumptionTime ].
	millisecondNextTick := millisecondNextTick min: SmallInteger maxVal.
	^millisecondNextTick.
