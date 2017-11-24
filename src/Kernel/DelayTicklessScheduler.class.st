"
This is a tickless scheduler. In case of no scheduled delay it will ask the VM to sleep indefinitely otherwise it sleep until the scheduled delay is due.

In contrast to the DelayMicrosecondScheduler the sleep duration will not be capped to one second.
"
Class {
	#name : #DelayTicklessScheduler,
	#superclass : #DelayMicrosecondScheduler,
	#category : #'Kernel-Processes'
}

{ #category : #'timer process' }
DelayTicklessScheduler >> handleTimerEvent: microsecondNowTick [
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
		scheduledDelay := nil ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil ].

	"Signal any expired delays"
	[ activeDelay notNil and:[ microsecondNowTick >= activeDelay schedulerResumptionTime ]] whileTrue: [
		activeDelay schedulerSignalWaitingProcess.
		activeDelay := suspendedDelays removeFirstOrNil.
		].

	"And signal when the next request is due. We trust our VM and will
	sleep as long as needed. microsecondNextTick = 0 will turn off the
	timer interrupt."
	microsecondNextTick := 0.
	activeDelay ifNotNil: [
		microsecondNextTick := activeDelay schedulerResumptionTime ].
	^microsecondNextTick.

]
